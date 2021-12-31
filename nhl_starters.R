library(tidyverse)

# download goalie stats from nhl.com/stats/goalies
# yeah it can probably be scraped, no I have not done that yet

goalies <- readxl::read_xlsx("goalies.xlsx") |>
  janitor::clean_names()

chart <- goalies |>
  group_by(team) |>
  summarize(
    team_games = sum(gs, na.rm = TRUE),
    .groups = "drop"
  ) |>
  right_join(goalies, by = "team") |>
  left_join(hockeyR::team_logos_colors, by = c("team" = "team_abbr")) |>
  filter(!is.na(team_logo_espn)) |>
  mutate(
    start_percent = round(gs / team_games * 100,1)
  ) |>
  arrange(-start_percent) |>
  group_by(team) |>
  mutate(
    max_starter = max(start_percent),
    cum_start_percent = cumsum(start_percent),
    x_lab = ifelse(
      start_percent == max_starter, start_percent / 2,
      (start_percent / 2) + lag(cum_start_percent)
    ),
    # alternate goalie colors
    goalie_num = row_number(),
    color = ifelse(goalie_num %% 2 != 0, team_color1, team_color2)
  ) |>
  ungroup() |>
  separate(player, into = c("first_name","last_name"), sep = " ", remove = FALSE)

# colors that are light enough to need black text
light_colors <- c("#FFB81C", "#FCB514", "#99D9D9", "#FFFFFF", "#F1BE48", "#A2AAAD", "#DDCBA4")

chart |>
  ggplot() +
  geom_col(
    aes(start_percent, reorder(team, max_starter),
        color = as.factor(start_percent), # color by start_percent to stack bars
        fill = color), #  add team color to fill
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_color_manual(values = rep("white",nrow(chart))) +
  geom_text(
    aes(x_lab, reorder(team, max_starter), label = last_name),
    color = ifelse(chart$color %in% light_colors, "black", "white")
  ) +
  ggimage::geom_image(
    data = chart |> select(team, max_starter, team_logo_espn) |> distinct(),
    aes(-2, reorder(team, max_starter), image = team_logo_espn),
    size = 0.025, asp = 16/11
  ) +
  ggimage::geom_image(
    data = chart |> select(team, max_starter, team_logo_espn) |> distinct(),
    aes(102, reorder(team, max_starter), image = team_logo_espn),
    size = 0.025, asp = 16/11
  ) +
  scale_x_continuous(
    limits = c(-4, 104), expand = c(0,0),
    breaks = seq(0,100,25),
    labels = paste0(seq(0,100,25),"%")
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray"),
    plot.background = element_rect(fill = "gray"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 18, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = .5)
  ) +
  labs(
    x = "% of team games started", y = NULL,
    title = "Goalie tandems & starters in the NHL",
    subtitle = paste0("through ", Sys.Date()-1),
    caption = "data: NHL.com | chart: Dan Morse"
  )
ggsave("goalies.png", width = 16, height = 11)
