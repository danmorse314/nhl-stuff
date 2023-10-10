# calculating distance traveled for each NHL team in 2022-23

library(tidyverse)
library(geosphere)

season <- 2024
int_games <- FALSE #  no international games for 2023-24

# get season games from NHL API
game_ids <- hockeyR::get_game_ids(season = season)

# double up games so each team has 82 games, 82 opponents
full_sched <- game_ids |>
  select(date, team = home_name, opponent = away_name) |>
  mutate(is_home = 1) |>
  bind_rows(
    game_ids |>
      select(date, team = away_name, opponent = home_name) |>
      mutate(is_home = 0)
  ) |>
  arrange(team, date)

# fix habs because arenas file doesn't do the accent
full_sched <- full_sched |>
  mutate(
    team = ifelse(team == "Montréal Canadiens", "Montreal Canadiens", team),
    opponent = ifelse(opponent == "Montréal Canadiens", "Montreal Canadiens", opponent)
  )

# add stop at home at beginning of year, christmas, and all-star break
first_game <- min(full_sched$date)

start_df <- tibble(
  date = as.character(as.Date(first_game)-1),
  team = unique(full_sched$team),
  opponent = "none",
  is_home = 1
)

xmas <- tibble(
  date = paste0(season,"-12-25"),
  team = unique(full_sched$team),
  opponent = "none",
  is_home = 1
)

asg <- tibble(
  date = paste0(season,"02-04"),
  team = unique(full_sched$team),
  opponent = "none",
  is_home = 1
)

full_sched <- bind_rows(
  full_sched, start_df, xmas, asg
) |>
  arrange(team, date)

# get arena locations
arenas <- read_csv("data/nhl_arena_coords.csv", col_types = cols())

# add arena locations
full_sched <- full_sched |>
  mutate(
    home_team = ifelse(is_home == 1, team, opponent)
  ) |>
  left_join(
    arenas, by = c("home_team" = "team")
  )

if(int_games){
  # add european games manually
  sj_nsh <- ggmap::geocode("O2 Arena Czechia")
  cbj_col <- ggmap::geocode("Nokia Arena")
  
  # there's a better way to do this but I'm tired
  full_sched <- full_sched |>
    mutate(
      lon = ifelse(
        date %in% c("2022-10-07","2022-10-08") &
          team %in% c("Nashville Predators", "San Jose Sharks"),
        sj_nsh$lon, lon
      ),
      lat = ifelse(
        date %in% c("2022-10-07","2022-10-08") &
          team %in% c("Nashville Predators", "San Jose Sharks"),
        sj_nsh$lat, lat
      ),
      arena = ifelse(
        date %in% c("2022-10-07","2022-10-08") &
          team %in% c("Nashville Predators", "San Jose Sharks"),
        "O2 Arena Czechia", arena
      ),
      lon = ifelse(
        date %in% c("2022-11-04","2022-11-05") &
          team %in% c("Columbus Blue Jackets", "Colorado Avalanche"),
        cbj_col$lon, lon
      ),
      lat = ifelse(
        date %in% c("2022-11-04","2022-11-05") &
          team %in% c("Columbus Blue Jackets", "Colorado Avalanche"),
        cbj_col$lat, lat
      ),
      arena = ifelse(
        date %in% c("2022-11-04","2022-11-05") &
          team %in% c("Columbus Blue Jackets", "Colorado Avalanche"),
        "Nokia Arena", arena
      ),
    )
}

# calculate distances between each game
full_sched <- full_sched |>
  group_by(team) |>
  mutate(
    distance = distHaversine(
      bind_cols(lon, lat),
      bind_cols(lag(lon), lag(lat))
    ),
    distance_km = round(distance/1000,1),
    distance_mi = round(distance/1609.34,1)
  ) |>
  ungroup()

# sum it up
travel <- full_sched |>
  group_by(team) |>
  summarize(
    distance_km = sum(distance_km, na.rm = TRUE),
    distance_mi = sum(distance_mi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(-distance_mi) |>
  left_join(hockeyR::team_logos_colors, by = c("team" = "full_team_name"))

if(int_games){
  sj <- filter(full_sched, team == "San Jose Sharks")
  # to europe and back = 11688.8 miles
  nsh <- filter(full_sched, team == "Nashville Predators")
  # to europe and back = 9604.2 miles
  col <- filter(full_sched, team == "Colorado Avalanche")
  # to europe and back = 8914.2 miles (from NYI to Europe, back to COL)
  cbj <- filter(full_sched, team == "Columbus Blue Jackets")
  # to europe and back = 8378 miles (from NJD to Europe, back to CBJ)
  
  travel_wo_europe <- travel |>
    mutate(
      distance_mi = case_when(
        team == "San Jose Sharks" ~ distance_mi - 11688.8,
        team == "Nashville Predators" ~ distance_mi - 9604.2,
        team == "Colorado Avalanche" ~ distance_mi - 8914.2,
        team == "Columbus Blue Jackets" ~ distance_mi - 8378,
        TRUE ~ distance_mi
      ),
      distance_km = round(distance_mi * 1.60934, 1)
    ) |>
    arrange(-distance_mi)
}

travel |>
  ggplot(aes(reorder(team, -distance_mi), distance_mi)) +
  geom_col(aes(fill = team_color1, color = team_color2)) +
  geom_hline(
    yintercept = mean(travel$distance_mi), color = "red", linetype = "dashed"
  ) +
  geom_text(
    aes(x = 32, y = mean(distance_mi) + 400, label = paste0(season-1,"-",substr(season,3,4),"Average")),
    color = "red", hjust = 1, vjust = 0
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(
    breaks = seq(0,60000,10000),
    labels = scales::comma,
    sec.axis = sec_axis(~.*1.60934, name = "Travel Distance (km)", labels = scales::comma)
    ) +
  ggimage::geom_image(
    aes(y = -1500, image = team_logo_espn),
    size = .04, asp = 1.5
  ) +
  # add global series logo for 2022-23
  #ggimage::geom_image(
  #  aes(y = europe, image = "https://i.imgur.com/hTo2xXq.gif"),
  #  size = .04, asp = 1.5
  #) +
  #geom_label(
  #  aes(x = 32.4, y = 52500, label = "          includes travel\n          to Europe"),
  #  hjust = 1, fill = "lightgray"
  #) +
  #ggimage::geom_image(
  #  aes(x = 27, y = 52500, image = "https://i.imgur.com/hTo2xXq.gif"),
  #  size = .06, asp = 1.5
  #) +
  #ggimage::geom_image(
  #  aes(y = distance_mi + 2000, image = team_logo_espn),
  #  size = .06, asp = 1.5
  #) +
  theme(
    panel.background = element_rect(fill = "gray"),
    plot.background = element_rect(fill = "gray"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray"),
    panel.grid.minor.y = element_line(color = "lightgray"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = NULL, y = "Travel Distance (miles)",
    title = glue::glue("Distance Traveled by every NHL team during the {season-1}-{(substr(season,3,4))} regular season"),
    subtitle = "includes stops at home for Christmas & All-Star break",
    caption = "chart: Dan Morse | data: hockeyR"
  )
ggsave(paste0("figures/nhl_travel_",season-1,"-",substr(season,3,4),".png"), width = 9, height = 6, dpi = 500)

full_sched |> write_csv(paste0("data/",season-1,"_",substr(season,3,4),"_nhl_schedule_full.csv"))
travel |> write_csv(paste0("data/",season-1,"_",substr(season,3,4),"_nhl_travel.csv"))

#############################################################

# tables
# it's her job okAY

library(gt)

table_data <- travel |>
  mutate(rank = row_number()) |>
  select(rank, team = team_logo_espn, distance_mi, distance_km)

tab1_data <- table_data |>
  slice(1:16)

tab2_data <- table_data |>
  slice(17:32)

tab1 <- tab1_data |>
  gt() |>
  text_transform(
    locations = cells_body(columns = team),
    fn = function(x) {
      web_image(
        url = tab1_data$team,
        height = 40
      )
    }
  ) |>
  cols_label(
    distance_mi = "Distance (miles)",
    distance_km = "Distance (km)"
  ) |>
  fmt_number(
    columns = distance_mi:distance_km, decimals = 0
  ) |>
  cols_align(align = "center") |>
  tab_header(
    title = "2022-23 NHL Team Distance Traveled"
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = distance_mi,
    domain = c(min(travel$distance_mi), max(travel$distance_mi)),
    reverse = TRUE
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = distance_km,
    domain = c(min(travel$distance_km), max(travel$distance_km)),
    reverse = TRUE
  ) |>
  #tab_footnote(
  #  footnote = "includes travel to Europe",
  #  locations = cells_body(
  #    columns = c(team),
  #    rows = c(1,2,3,12)
  #  )
  #) |>
  tab_source_note(source_note = "| ") |>
  gtExtras::gt_theme_espn()

tab2 <- tab2_data |>
  gt() |>
  text_transform(
    locations = cells_body(columns = team),
    fn = function(x) {
      web_image(
        url = tab2_data$team,
        height = 40
      )
    }
  ) |>
  cols_label(
    distance_mi = "Distance (miles)",
    distance_km = "Distance (km)"
  ) |>
  cols_align(align = "center") |>
  fmt_number(
    columns = distance_mi:distance_km, decimals = 0
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = distance_mi,
    domain = c(min(travel$distance_mi), max(travel$distance_mi)),
    reverse = TRUE
  ) |>
  gtExtras::gt_hulk_col_numeric(
    columns = distance_km,
    domain = c(min(travel$distance_km), max(travel$distance_km)),
    reverse = TRUE
  ) |>
  tab_header(
    title = "(w/stops at home for Xmas & All-Star break)"
  ) |>
  tab_source_note(source_note = "table: Dan Morse | data: hockeyR") |>
  gtExtras::gt_theme_espn()

tabs <- list(tab1,tab2)

gtExtras::gt_two_column_layout(tabs, output = "save",
                               filename = paste0("figures/nhl_travel_table_",season-1,"_",substr(season,3,4),".png"),
                               vwidth = 950,
                               expand = 50)

#########################################################

# maps

#full_sched <- read_csv("data/2022_23_nhl_schedule_full.csv", col_types = cols())
#travel <- read_csv("data/2022_23_nhl_travel.csv", col_types = cols())

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::theme_set(theme_bw())

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
    title = paste(tm,season-1,"-",substr(season,3,4),"\nTravel Map")
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

ggsave(paste0("figures/maps_",season,"/",tm,"_travel_map.png"),width = 9, height = 6, dpi = 500)

# get em all
for(tm in unique(arenas$team)) {
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
      title = paste(tm,season-1,"-",substr(season,3,4),"\nTravel Map")
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
  
  ggsave(paste0("figures/maps_",season,"/",tm,"_travel_map.png"),width = 9, height = 6, dpi = 500)
  
}
