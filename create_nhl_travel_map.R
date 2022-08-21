library(tidyverse)
library(geosphere)

full_sched <- read_csv(
  "https://github.com/danmorse314/nhl-stuff/raw/main/data/2022_23_nhl_schedule_full.csv",
  col_types = cols()
  )
arenas <- read_csv(
  "https://github.com/danmorse314/nhl-stuff/raw/main/data/nhl_arena_coords.csv",
  col_types = cols()
  )

travel <- read_csv(
  "https://github.com/danmorse314/nhl-stuff/raw/main/data/2022_23_nhl_travel.csv",
  col_types = cols()
  )

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

arena_df <- arenas |>
  left_join(hockeyR::team_logos_colors, by = c("team" = "full_team_name"))

# enter desired team map
tm <- "Seattle Kraken"

df <- full_sched |>
  filter(team == tm)

rink <- filter(df, home_team == tm) |>
  slice(1) |>
  pull(arena)

tm_curves <- df |>
  filter(lead(lon) != lon & lead(lat) != lat) |>
  mutate(trip = ifelse(lead(arena) == rink, "Return home","To opponent")) |>
  mutate(trip = ifelse(is.na(trip), "To opponent", trip))

world |>
  ggplot() +
  geom_sf(fill = "antiquewhite") +
  coord_sf(xlim = c(-125,-70), ylim = c(25,55)) +
  #geom_path(
  #  data = sea,
  #  aes(lon, lat)
  #) +
  geom_curve(
    data = tm_curves,
    aes(x = lon, xend = lead(lon),
        y = lat, yend = lead(lat),
        color = trip),
    curvature = .2, size = 1.5
  ) +
  geom_point(
    data = arena_df, aes(lon, lat),
    color = arena_df$team_color1, size = 2
  ) +
  scale_color_manual(
    values = c(
      filter(travel, team == tm)$team_color2,
      filter(travel, team == tm)$team_color1
    )
  ) +
  #borders("state") +
  ggimage::geom_image(
    data = filter(arena_df, team == tm),
    aes(lon, lat, image = team_logo_espn),
    size = .05, asp = 1.5
  ) +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = paste(tm,"2022-23\nTravel Map")
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(.1,.1),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = .95, vjust = -15, face = "bold", size = 20),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )
