##### Pulls data out of lineup.csv and prepares for use by app.R #####

# Libraries
library(spotifyr)
library(dplyr)
library(readr)
library(yaml)
library(tidyr)
library(purrr)

# Setup API credentials
credentials <- read_yaml('credentials.yaml')
Sys.setenv(SPOTIFY_CLIENT_ID = credentials$id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials$secret)

# Read in lineup
slots <- lineup <- read.csv('lineup.csv', na.strings = "") %>%
  mutate(Slot = row_number())

# Function to try and find an artist's ID
search_artist <- function(artist) {
  out <- tryCatch( {
    return(search_spotify(artist, type = "artist", limit = 1) %>% pull(id))
  },
  error=function(cond) { 
    return(NA)
  })
  
  return(out)
}

# Get artists playing in each slot
artists <- slots %>%
  pivot_longer(Artist:Artist.6, values_drop_na = TRUE, values_to = "Artist") %>%
  group_by(Slot) %>%
  filter(n() == 1 | name != "Artist") %>%
  select(Slot, Artist) %>%
  rowwise() %>%
  mutate(id = search_artist(Artist)) %>%
  filter(!is.na(id)) %>%
  ungroup() %>%
  # Drop incorrect matches
  filter(! id %in% c("2SrSdSvpminqmStGELCSNd"))

# Remove extra details from slots
slots <- slots %>%
  filter(Slot %in% artists$Slot) %>%
  mutate(startSort = as.POSIXct(paste("2000-01-01", Start), format="%Y-%m-%d %H:%M"),
         startMorning = lubridate::hour(startSort) < 6) %>%
  arrange(factor(Day, levels = c("Thursday", "Friday", "Saturday", "Sunday")), startMorning, Start) %>%
  select(Stage, Day, Start, Finish, Artist, Slot)

# Get top 5 tracks for each artist
trackstmp <- map_dfr(unique(artists$id), ~get_artist_top_tracks(.x) %>% head(5))

# Get artist IDs for each top track
trackartists <- trackstmp %>%
  unnest(artists, names_sep = ".") %>%
  distinct(id, artists.id)

tracks <- trackstmp %>%
  # Pull out artist and artwork details for each track
  unnest(album.images, names_sep = ".") %>%
  filter(album.images.height == 300) %>%
  unnest(artists, names_sep = ".") %>%
  group_by(id) %>%
  summarise(name = first(name),
            artwork = first(album.images.url),
            artists = paste(unique(artists.name), collapse = ", ")) %>%
  mutate(artwork = paste0('<img src="', artwork, '" height="52"></img>')) %>%
  # Join in artist IDs
  left_join(trackartists, by = "id")

# Write data
write_rds(slots, "slots.RDS")
write_rds(artists, "artists.RDS")
write_rds(tracks, "tracks.RDS")
