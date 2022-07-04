# Wrangling libraries
library(yaml)
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr) 
library(purrr)
library(tidyr)

# Visualisation libraries
library(shiny)
library(shinydashboard)
library(DT)
require(visNetwork)
library(timevis)
library(shinyjs)

# What is the URL of the app
APP_URL <- if (interactive()) {
  options(shiny.port = 8100)
  "http://localhost:8100/"
} else {
  "https://lukebandy.shinyapps.io/FestScan/"
}

# Read app ID and secret
credentials <- read_yaml('credentials.yaml')

# Generate authentication url based on app ID and URL
auth_url <- sprintf("https://accounts.spotify.com/authorize?client_id=%s&scope=%s&redirect_uri=%s&response_type=code",
                    utils::URLencode(credentials$id, reserved = TRUE, repeated = TRUE),
                    utils::URLencode("user-top-read%20playlist-modify-public", reserved = TRUE, repeated = TRUE),
                    utils::URLencode(APP_URL, reserved = TRUE, repeated = TRUE)
)

# Read in data
slots <- readRDS("slots.RDS")
artists <- readRDS("artists.RDS")
stages <- read.csv("stages.csv") %>%
  mutate(id = row_number())
tracks <- readRDS("tracks.RDS")

# Function to get user's top tracks for a given time range
get_my_top_artists_or_tracks <- function(time_range, token) {
  res <- RETRY("GET", "https://api.spotify.com/v1/me/top/artists",
               add_headers(Authorization = token),
               body = c(limit = "50",
                        time_range = time_range))
  
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE)
  
  res$items
}

# Function to get an artist's related artists
get_related_artists <- function(id, token) {
  res <- RETRY("GET", paste0("https://api.spotify.com/v1/artists/", id, "/related-artists"),
               add_headers(Authorization = token))
  
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE)
  
  res$artists
}

# Use javascript to open a Spotify playlist
jsCode <- "
shinyjs.openPlaylist = function(id) {
  window.open('https://open.spotify.com/playlist/' + id, '_blank').focus();
}"

# Render dashboard
ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "FestScan"
  ),
  
  dashboardSidebar(
    # Spotify logo
    # TODO: Move text right
    "Powered by", br(),
    img(src='Spotify_Logo_RGB_White.png', 
        width="100%"),
    
    # Festival selection
    selectInput(
      "festival",
      "Festival",
      c("Glastonbury 2022")
    ),
    # TODO: Move text right
    "Currently includes Main Stages, Silver Hayes, The Park, Glade, Block9, The Common and Shangri-La",
    
    # Day selection
    selectizeInput(
      "days",
      "Days",
      c("Thursday", "Friday", "Saturday", "Sunday"),
      selected = c("Thursday", "Friday", "Saturday", "Sunday"),
      multiple = TRUE
    ),
    
    # Playlist controls
    sliderInput("playlistSize", "Playlist Size", min = 25, 
                max = 75, value = 50),
    actionButton("playlistGenerate", "Generate Playlist"),
    actionButton("playlistSave", "Save Playlist"),
    # TODO: Why is the download button a big left?
    downloadButton("download", "Download Schedule", class = "skin-blue"),
    # TODO: Why has this CSS stopped working?
    tags$head(tags$style(".skin-blue .sidebar a { color: #444; }"))
  ),
  
  dashboardBody(
    
    # Setup javascript
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("openPlaylist")),
    
    fluidRow(
      box(
        width = 4,
        "FestScan builds a personalised Glastonbury 2022 schedule and playlist 
        based on your Spotify listening history.
        
        The size of the playlist can be adjusted, and saved to your Spotify account
        when you are happy.
        
        The network graph can be used to explore why the app has recommended you 
        each artist.
        
        If needed, an Excel workbook of your personalise schedule can be downloaded."
      ),
      # Schedule visuals
      tabBox(
        width = 8,
        tabPanel(
          "Thursday",
          timevisOutput("thursday")
        ),
        tabPanel(
          "Friday",
          timevisOutput("friday")
        ),
        tabPanel(
          "Saturday",
          timevisOutput("saturday")
        ),
        tabPanel(
          "Sunday",
          timevisOutput("sunday")
        )
      )
    ),
    fluidRow(
      # Playlist table visual
      box(
        DTOutput('playlist')
      ),
      # Artist network visual
      box(
        visNetworkOutput("network")
      )
    )
  )
)

uiFunc <- function(req) {
  
  # If no code is supplied, redirect user to login page 
  if (is.null(parseQueryString(req$QUERY_STRING)$code)) {
    return(tags$script(HTML(sprintf("location.replace(\"%s\");", auth_url))))
  } 
  
  # Otherwise, display ui
  else {
    ui
  }
}

server <- function(input, output, session) {
  
  # Get URL query parameters
  params <- parseQueryString(isolate(session$clientData$url_search))
  
  # If there is no code in the URL, end
  if (is.null(params$code)) {
    return()
  }
  
  # Encode application id and secret
  token <- stringi::stri_enc_toascii(paste0(credentials$id, ":", credentials$secret))
  token <- RCurl::base64Encode(token)
  token <- paste('Basic', token)
  
  # Get an API token based on the access token in the URL
  token <- POST('https://accounts.spotify.com/api/token',
                encode = "form",
                add_headers(Authorization = token),
                body = list(
                  grant_type = 'authorization_code',
                  redirect_uri = APP_URL,
                  code = params$code
                ))
  
  # Extract token from response
  token <- paste("Bearer", content(token)$access_token)
  
  # Which slots are within the selected days
  slots_filtered <- reactive({
    slots %>%
      filter(Day %in% input$days)
  })
  
  # Which artists are plying the selected days
  artists_filtered <- reactive({
    artists %>% 
      filter(Slot %in% slots_filtered()$Slot)
  })
  
  # Get Spotify's recommended artists
  recommended <- reactive({
    
    # Setup progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    # Get favourite artists
    progress$set(message = "Getting your favourite artists...", value = 0)
    artists1 <- 
      bind_rows(
        get_my_top_artists_or_tracks(time_range = 'long_term', token = token),
        get_my_top_artists_or_tracks(time_range = 'medium_term', token = token),
        get_my_top_artists_or_tracks(time_range = 'short_term', token = token)) %>%
      distinct() %>%
      mutate(level = 1)
    
    # Get related artists
    progress$set(message = "Getting your recommended artists...", value = 0.2)
    artists2 <- artists1 %>%
      pull(id) %>%
      map_dfr(~ get_related_artists(.x, token) %>% 
                as_tibble() %>%
                mutate(parent = .x)) %>%
      anti_join(artists1, by = "id") %>%
      mutate(level = 2)
    
    # Get artists related to related artists
    progress$set(message = "Getting your recommended artists...", value = 0.6)
    artists3 <- artists2 %>%
      distinct(id) %>%
      sample_frac(0.4) %>%
      pull(id) %>%
      map_dfr(~ get_related_artists(.x, token) %>% 
                as_tibble() %>% 
                mutate(parent = .x)) %>%
      anti_join(artists1, by = "id") %>%
      anti_join(artists2, by = "id") %>%
      mutate(level = 3)
    
    # Return combined artists list
    progress$set(message = "Getting your recommended artists...", value = 1)
    bind_rows(artists1, artists2, artists3)
    
  })
  
  # Generate playlist based on recommended artists
  playlist <- reactive({
    
    # Generate new playlist whenever button pressed
    input$playlistGenerate
    
    # Get recommended artists that are playing
    r <- recommended() %>% 
      filter(id %in% artists_filtered()$id) %>%
      pull(id)
    
    # Filter tracks for those by playing+recommended artists
    t <- tracks %>%
      filter(artists.id %in% r) %>%
      distinct(id, name, artwork, artists)
    
    # Sample possible tracks
    sample_n(t, min(nrow(t), input$playlistSize))
    
  })
  
  # Save playlist to user's account
  observeEvent(input$playlistSave, {
    
    # Get user ID
    u <- RETRY("GET", "https://api.spotify.com/v1/me",
                 add_headers(Authorization = token))
    
    u <- fromJSON(content(u, as = "text", encoding = "UTF-8"), 
                    flatten = TRUE)
    
    # Create playlist
    p <- RETRY("POST", paste0("https://api.spotify.com/v1/users/", u$id, "/playlists"),
                 add_headers(Authorization = token),
                 body = list(name = "Glastonbury 2022",
                          description = "Generated by FestScan",
                          public = TRUE,
                          collaborative = FALSE), 
               encode = "json")
    
    p <- fromJSON(content(p, as = "text", encoding = "UTF-8"), 
                    flatten = TRUE)
    
    # Add tracks to playlist
    RETRY("POST", paste0("https://api.spotify.com/v1/playlists/", p$id, "/tracks?uris=", paste0(mutate(playlist(), uri=paste0("spotify:track:", id)) %>% pull(uri), collapse = ",")),
          add_headers(Authorization = token), 
          encode = "json")
    
    # Open playlist
    js$openPlaylist(p$id)
    
  })
  
  # Download excel workbook of recommended slots
  output$download <- downloadHandler(
    filename = function() {
      "Glastonbury 2022.xlsx"
    },
    content = function(con) {
      # Get recommend slots
      s <- artists %>% filter(id %in% recommended()$id) %>% pull(Slot)
      
      # Write workbook 
      # TODO: Pivot times wide or something more interesting
      openxlsx::write.xlsx(list("Thursday" = slots %>% filter(Day == "Thursday", Slot %in% s) %>% select(-c(Day, Slot)),
                                "Friday" = slots %>% filter(Day == "Friday", Slot %in% s) %>% select(-c(Day, Slot)),
                                "Saturday" = slots %>% filter(Day == "Saturday", Slot %in% s) %>% select(-c(Day, Slot)),
                                "Sunday" = slots %>% filter(Day == "Sunday", Slot %in% s) %>% select(-c(Day, Slot))),
                           file = con)
    }
  )
  
  # Render Thursday schedule
  # TODO: Functionise schedule code 
  output$thursday <- renderTimevis({
    
    # DataFrame of slots
    s <- slots %>%
      # Filter for relevant slots
      filter(!is.na(Start), 
             !is.na(Finish), 
             Day == "Thursday",
             Slot %in% (artists %>% filter(id %in% pull(recommended(), id)) %>% pull(Slot))) %>%
      # Add stage group id
      left_join(stages %>%
                  select(content = Stage, group = id),
                by = c("Stage" = "content")) %>%
      select(content = Artist, start = Start, end = Finish, group) %>%
      # Adjust datas
      mutate(start = as.POSIXct(paste("2022-06-23", start), format="%Y-%m-%d %H:%M"),
             end = as.POSIXct(paste("2022-06-23", end), format="%Y-%m-%d %H:%M"),
             startnd = hour(start) <= 6,
             start = if_else(startnd, start + 86400, start),
             endnd = hour(end) <= 6,
             end = if_else(endnd, end + 86400, end))
    
    # Visualise
    timevis(s, groups = stages %>% select(id, content = Stage) %>% filter(id %in% s$group),
            options = list(zoomable = FALSE))
    
  })
  
  # Render Friday schedule
  # TODO: Functionise schedule code 
  output$friday <- renderTimevis({
    
    # DataFrame of slots
    s <- slots %>%
      # Filter for relevant slots
      filter(!is.na(Start), 
             !is.na(Finish), 
             Day == "Friday",
             Slot %in% (artists %>% filter(id %in% pull(recommended(), id)) %>% pull(Slot))) %>%
      # Add stage group id
      left_join(stages %>%
                  select(content = Stage, group = id),
                by = c("Stage" = "content")) %>%
      select(content = Artist, start = Start, end = Finish, group) %>%
      # Adjust datas
      mutate(start = as.POSIXct(paste("2022-06-24", start), format="%Y-%m-%d %H:%M"),
             end = as.POSIXct(paste("2022-06-24", end), format="%Y-%m-%d %H:%M"),
             startnd = hour(start) <= 6,
             start = if_else(startnd, start + 86400, start),
             endnd = hour(end) <= 6,
             end = if_else(endnd, end + 86400, end))
    
    # Visualise
    timevis(s, groups = stages %>% select(id, content = Stage) %>% filter(id %in% s$group),
            options = list(zoomable = FALSE))
    
  }) 
  
  # Render Saturday schedule
  # TODO: Functionise schedule code 
  output$saturday <- renderTimevis({
    
    # DataFrame of slots
    s <- slots %>%
      # Filter for relevant slots
      filter(!is.na(Start), 
             !is.na(Finish), 
             Day == "Saturday",
             Slot %in% (artists %>% filter(id %in% pull(recommended(), id)) %>% pull(Slot))) %>%
      # Add stage group id
      left_join(stages %>%
                  select(content = Stage, group = id),
                by = c("Stage" = "content")) %>%
      select(content = Artist, start = Start, end = Finish, group) %>%
      # Adjust datas
      mutate(start = as.POSIXct(paste("2022-06-25", start), format="%Y-%m-%d %H:%M"),
             end = as.POSIXct(paste("2022-06-25", end), format="%Y-%m-%d %H:%M"),
             startnd = hour(start) <= 6,
             start = if_else(startnd, start + 86400, start),
             endnd = hour(end) <= 6,
             end = if_else(endnd, end + 86400, end))
    
    # Visualise
    timevis(s, groups = stages %>% select(id, content = Stage) %>% filter(id %in% s$group),
            options = list(zoomable = FALSE))
    
  }) 
  
  # Render Sunday schedule
  # TODO: Functionise schedule code 
  output$sunday <- renderTimevis({
    
    # DataFrame of slots
    s <- slots %>%
      # Filter for relevant slots
      filter(!is.na(Start), 
             !is.na(Finish), 
             Day == "Sunday",
             Slot %in% (artists %>% filter(id %in% pull(recommended(), id)) %>% pull(Slot))) %>%
      # Add stage group id
      left_join(stages %>%
                  select(content = Stage, group = id),
                by = c("Stage" = "content")) %>%
      select(content = Artist, start = Start, end = Finish, group) %>%
      # Adjust datas
      mutate(start = as.POSIXct(paste("2022-06-26", start), format="%Y-%m-%d %H:%M"),
             end = as.POSIXct(paste("2022-06-26", end), format="%Y-%m-%d %H:%M"),
             startnd = hour(start) <= 6,
             start = if_else(startnd, start + 86400, start),
             endnd = hour(end) <= 6,
             end = if_else(endnd, end + 86400, end))
    
    # Visualise
    timevis(s, groups = stages %>% select(id, content = Stage) %>% filter(id %in% s$group),
            options = list(zoomable = FALSE))
    
  }) 
  
  # Render table of generated playlist
  output$playlist <- renderDataTable({
    # Add hyperlinks and polish column names
    playlist() %>% 
      mutate(name = paste0("<a href='https://open.spotify.com/track/", id, "' target='blank'>", name, "</a>"),
             artwork = paste0("<a href='https://open.spotify.com/track/", id, "' target='blank'>", artwork, "</a>")) %>%
      select(Artists = artists, Track = name, Artwork = artwork)
  }, 
  escape = FALSE, # Enable HTML contents
  selection = 'none',
  rownames = FALSE,
  options = list(dom = 'ftp', pageLength = 5)
  )
  
  # Render artist network
  output$network <- renderVisNetwork({
    
    # Get tedious link artists that are playing
    nodes3 <- recommended() %>%
      filter(level == 3,
             id %in% pull(artists_filtered(), id))
    
    # Get related artists that are playing or who are related to tedious link artists that are
    nodes2 <- recommended() %>%
      filter(level == 2,
             (id %in% nodes3$parent) | (id %in% pull(artists_filtered(), id)))
    
    # Get favourite artists that are playing, or if has a related artist that is showing
    nodes1 <- recommended() %>%
      filter(level == 1,
             id %in% nodes2$parent | id %in% pull(artists_filtered(), id)) %>%
      mutate(shape = "star")
    
    # Combine nodes
    nodes <- bind_rows(nodes1, nodes2, nodes3) %>%
      distinct(id, label = name, value = popularity, shape) %>%
      # Add in stage colour
      left_join(artists_filtered() %>% 
                  left_join(slots_filtered(), by = "Slot") %>%
                  left_join(stages %>% select(Stage, Colour), by = "Stage") %>% 
                  select(id, color = Colour) %>%
                  group_by(id) %>%
                  summarise(color = first(color)), 
                by = "id") %>%
      # Adjust node size and colour
      mutate(value = value/10,
             color = replace_na(color, "grey"))
    
    # Connections between nodes
    edges <- bind_rows(nodes2, nodes3) %>% 
      filter(id %in% nodes$id) %>%
      select(from = parent, to = id)
    
    # Visualise
    visNetwork(nodes, edges) %>%
      # Add legend
      # TODO: Automatically get from stages.csv
      visLegend(addNodes = list(
        list(label = "Favourite", shape = "star"),
        list(label = "Not Playing", color = "gray"),
        list(label = "Main Stages", color = "darkred"),
        list(label = "Silver Hayes", color = "purple"),
        list(label = "The Park", color = "green"),
        list(label = "Glade", color = "blue"),
        list(label = "Block9", color = "yellow"),
        list(label = "The Common", color = "orange"),
        list(label = "Shangri-La", color = "black")), 
        useGroups = FALSE) %>%
      # On click, open spotify artist page
      visEvents(selectNode = "function(properties) {
      window.open('https://open.spotify.com/artist/' + this.body.data.nodes.get(properties.nodes[0]).id, '_blank').focus();
      }")
  })
}

shinyApp(uiFunc, server)