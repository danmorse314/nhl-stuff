# gathering arena coordinates from google maps using arena names

library(tidyverse)
library(ggmap)

# gor accessing google maps to get locations
register_google(key = Sys.getenv("GOOGLE_API_KEY"))

arenas <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_National_Hockey_League_arenas") |>
  rvest::html_element("table") |>
  rvest::html_table() |>
  mutate(capacity = as.numeric(gsub(",","",Capacity))) |>
  select(
    arena = Arena,
    city = Location,
    team = Team,
    capacity
  )

arena_coords <- ggmap::geocode(arenas$arena)

arenas$lon <- arena_coords$lon
arenas$lat <- arena_coords$lat

# stars one was wrong
arenas <- arenas |>
  mutate(
    lat = ifelse(team == "Dallas Stars", 32.790527, lat),
    lon = ifelse(team == "Dallas Stars", -96.810883, lon)
  )

arenas |> write_csv("data/nhl_arena_coords.csv")
