# team cards
install.packages(c("tidyverse","patchwork","remotes","padr","zoo","gridExtra","ggimage","scales"))
remotes::install_github("danmorse314/hockeyR")
library(tidyverse)
library(patchwork)

# load pbp
pbp <- hockeyR::load_pbp(shift_events = TRUE)

# add variables
pbp <- pbp |>
  mutate(
    opponent = ifelse(event_team == home_name, away_name, home_name),
    zone = case_when(
      (x_fixed >= 25 & event_team == home_name) |
        (x_fixed <= -25 & event_team == away_name) ~ "OZ",
      (x_fixed <= -25 & event_team == home_name) |
        (x_fixed >= 25 & event_team == away_name) ~ "DZ",
      between(x_fixed, -24, 24) ~ "NZ"
    )
  )

# define shot types
corsi <- c("SHOT","MISSED_SHOT","BLOCKED_SHOT","GOAL")
fenwick <- c("SHOT","MISSED_SHOT","GOAL")

# time on ice
home_toi_5v5 <- pbp |>
  padr::pad_int("game_seconds", group = "game_id") |>
  tidyr::fill(c(strength_state, home_name)) |>
  ungroup() |>
  filter(strength_state == "5v5") |>
  group_by(team = home_name) |>
  summarize(
    toi_seconds = n(),
    toi_minutes = toi_seconds / 60,
    .groups = "drop"
  )

away_toi_5v5 <- pbp |>
  padr::pad_int("game_seconds", group = "game_id") |>
  tidyr::fill(c(strength_state, away_name)) |>
  ungroup() |>
  filter(strength_state == "5v5") |>
  group_by(team = away_name) |>
  summarize(
    toi_seconds = n(),
    toi_minutes = toi_seconds / 60,
    .groups = "drop"
  )

team_toi_5v5 <- bind_rows(home_toi_5v5, away_toi_5v5) |>
  group_by(team) |>
  summarize(
    toi = sum(toi_minutes),
    .groups = "drop"
  )

# calculate 5v5 stats
team_stats_5v5 <- pbp |>
  filter(period < 5 & !is.na(event_team) & strength_state == "5v5" & !is.na(event_team_abbr)) |>
  group_by(team = event_team, team_abbr = event_team_abbr) |>
  summarize(
    #gp = length(unique(game_id)),
    gf = sum(event_type == "GOAL"),
    ff = sum(event_type %in% fenwick),
    cf = sum(event_type %in% corsi),
    xgf = sum(xg, na.rm = TRUE),
    unblocked_rate = ff / cf,
    sh_perc = gf / ff,
    .groups = "drop"
  ) |>
  left_join(
    pbp |>
      filter(period < 5  & !is.na(event_team) & strength_state == "5v5") |>
      mutate(team_abbr = ifelse(event_team == home_name, away_abbreviation, home_abbreviation)) |>
      group_by(team = opponent, team_abbr) |>
      summarize(
        ga = sum(event_type == "GOAL"),
        fa = sum(event_type %in% fenwick),
        ca = sum(event_type %in% corsi),
        xga = sum(xg, na.rm = TRUE),
        blocked_rate = (ca - fa) / ca,
        gsax = xga - ga,
        .groups = "drop"
      ),
    by = c("team","team_abbr")
  ) |>
  left_join(team_toi_5v5, by = "team") |>
  mutate(
    cf_perc = 100 * cf / (cf + ca),
    xgf_perc = 100 * xgf / (xgf + xga),
    across(
      .cols = c(gf:xgf, ga:xga),
      ~60*.x / toi, .names = "{.col}_per_60"
    )
  ) |>
  arrange(-xgf_perc) |>
  left_join(
    hockeyR::team_logos_colors |>
      filter(status == "active") |>
      mutate(team_color2 = ifelse(team_color2 == "#FFFFFF","lightgray", team_color2)),
    by = "team_abbr"
    )

# just offensive stats
offense <- team_stats_5v5 |>
  select(
    team_abbr, team, team_logo_espn, team_color1, team_color2,
    gf_per_60, xgf_per_60, cf_per_60, unblocked_rate, sh_perc
  ) |>
  mutate(
    across(
      .cols = gf_per_60:sh_perc,
      ~scales::rescale(.x, c(0,1), c(min(.x), max(.x))),
      .names = "{.col}_scaled"
    )
  )

offense <- offense |>
  select(team_abbr:sh_perc) |>
  pivot_longer(
    cols = gf_per_60:sh_perc, names_to = "stat", values_to = "real_value"
  ) |>
  left_join(
    offense |>
      select(team_abbr, ends_with("scaled")) |>
      rename_with(
        .cols = ends_with("scaled"),
        ~str_remove(.x, "_scaled")
      ) |>
      pivot_longer(
        cols = gf_per_60:sh_perc, names_to = "stat", values_to = "scaled_value"
      ),
    by = c("team_abbr","stat")
  ) |>
  mutate(
    real_value = case_when(
      stat %in% c("gf_per_60","xgf_per_60") ~ as.character(round(real_value, 2)),
      stat %in% c("unblocked_rate","sh_perc") ~ as.character(paste0(round(100 * real_value, 1),"%")),
      stat == "cf_per_60" ~ as.character(round(real_value, 1))
    ),
    stat = factor(
      stat,
      levels = c("sh_perc","unblocked_rate","cf_per_60","xgf_per_60","gf_per_60")
    )
  )

# defensive stats
defense <- team_stats_5v5 |>
  select(
    team_abbr, team, team_logo_espn, team_color1, team_color2,
    ga_per_60, xga_per_60, ca_per_60, blocked_rate, gsax
  ) |>
  mutate(
    across(
      .cols = ga_per_60:ca_per_60,
      ~scales::rescale(.x, c(0,1), c(max(.x), min(.x))),
      .names = "{.col}_scaled"
    ),
    across(
      .cols = blocked_rate:gsax,
      ~scales::rescale(.x, c(0,1), c(min(.x), max(.x))),
      .names = "{.col}_scaled"
    )
  )

defense <- defense |>
  select(team_abbr:gsax) |>
  pivot_longer(
    cols = ga_per_60:gsax, names_to = "stat", values_to = "real_value"
  ) |>
  left_join(
    defense |>
      select(team_abbr, ends_with("scaled")) |>
      rename_with(
        .cols = ends_with("scaled"),
        ~str_remove(.x, "_scaled")
      ) |>
      pivot_longer(
        cols = ga_per_60:gsax, names_to = "stat", values_to = "scaled_value"
      ),
    by = c("team_abbr","stat")
  ) |>
  mutate(
    real_value = case_when(
      stat %in% c("ga_per_60","xga_per_60") ~ as.character(round(real_value, 2)),
      stat == "blocked_rate" ~ as.character(paste0(round(100 * real_value, 1),"%")),
      stat %in% c("ca_per_60","gsax") ~ as.character(round(real_value, 1))
    ),
    stat = factor(
      stat,
      levels = c("gsax","blocked_rate","ca_per_60","xga_per_60","ga_per_60")
    )
  )

# neutral zone
nz_stats <- pbp |>
  filter(period < 5 & !is.na(event_team) & strength_state == "5v5") |>
  filter(zone == "NZ") |>
  group_by(team = event_team) |>
  summarize(
    gives = sum(event_type == "GIVEAWAY"),
    takes = sum(event_type == "TAKEAWAY"),
    hits = sum(event_type == "HIT"),
    .groups = "drop"
  ) |>
  left_join(
    pbp |>
      filter(period < 5 & !is.na(event_team) & strength_state == "5v5") |>
      filter(zone == "NZ") |>
      group_by(team = opponent) |>
      summarize(
        opp_gives = sum(event_type == "GIVEAWAY"),
        opp_takes = sum(event_type == "TAKEAWAY"),
        opp_hits = sum(event_type == "HIT"),
        .groups = "drop"
      ),
    by = "team"
  ) |>
  left_join(
    team_toi_5v5, by = "team"
  ) |>
  mutate(
    nz_defense = (takes + opp_gives + hits) / toi,
    nz_offense = (opp_takes + gives + opp_hits) / toi,
    nz_diff = nz_defense - nz_offense
  ) |>
  select(team, nz_offense, nz_defense, nz_diff)

nz_stats <- nz_stats |>
  mutate(
    nz_offense_scaled = scales::rescale(
      nz_offense,
      c(0,1),
      c(max(nz_stats$nz_offense), min(nz_stats$nz_offense))
      ),
    nz_defense_scaled = scales::rescale(
      nz_defense,
      c(0,1),
      c(min(nz_stats$nz_defense), max(nz_stats$nz_defense))
    ),
    nz_diff_scaled = scales::rescale(
      nz_diff,
      c(0,1),
      c(min(nz_stats$nz_diff), max(nz_stats$nz_diff))
    )
  )

nz_stats <- nz_stats |>
  select(team:nz_diff) |>
  pivot_longer(
    cols = 2:4, names_to = "stat", values_to = "real_value"
  ) |>
  left_join(
    nz_stats |>
      select(team, ends_with("scaled")) |>
      rename_with(
        .cols = ends_with("scaled"),
        ~str_remove(.x, "_scaled")
      ) |>
      pivot_longer(
        cols = 2:4, names_to = "stat", values_to = "scaled_value"
      ),
    by = c("team","stat")
  ) |>
  mutate(
    stat = factor(
      stat,
      levels = c("nz_defense","nz_offense","nz_diff")
    )
  ) |>
  left_join(
    hockeyR::team_logos_colors |>
      filter(status == "active") |>
      mutate(
        full_team_name = ifelse(team_abbr == "MTL", "Montréal Canadiens", full_team_name)
      ),
    by = c("team" = "full_team_name")
  )

# standings info
standings <- hockeyR::get_standings() |>
  utils::type.convert(as.is = TRUE)

# Special Teams
toi_pp <- pbp |>
  mutate(pp_team = case_when(
    home_skaters > away_skaters ~ home_name,
    away_skaters > home_skaters ~ away_name,
    home_skaters == away_skaters ~ NA_character_
  )) |>
  padr::pad_int("game_seconds", group = "game_id") |>
  tidyr::fill(c(strength_state, pp_team)) |>
  ungroup() |>
  filter(
    strength_state %in% c("5v4","4v5","5v3","3v5","4v3","3v4")
  ) |>
  group_by(team = pp_team) |>
  summarize(
    toi_seconds = n(),
    toi_minutes = toi_seconds / 60,
    .groups = "drop"
  )

toi_pk <- pbp |>
  mutate(pk_team = case_when(
    home_skaters < away_skaters ~ home_name,
    away_skaters < home_skaters ~ away_name,
    home_skaters == away_skaters ~ NA_character_
  )) |>
  padr::pad_int("game_seconds", group = "game_id") |>
  tidyr::fill(c(strength_state, pk_team)) |>
  ungroup() |>
  filter(
    strength_state %in% c("5v4","4v5","5v3","3v5","4v3","3v4")
  ) |>
  group_by(team = pk_team) |>
  summarize(
    toi_seconds = n(),
    toi_minutes = toi_seconds / 60,
    .groups = "drop"
  )

pp <- pbp |>
  mutate(pp_team = case_when(
    home_skaters > away_skaters ~ home_name,
    away_skaters > home_skaters ~ away_name,
    home_skaters == away_skaters ~ NA_character_
  )) |>
  filter(strength_state %in% c("5v4","4v5","5v3","3v5","4v3","3v4")) |>
  group_by(team = pp_team) |>
  summarize(
    gf = sum(event_type == "GOAL"),
    xgf = sum(xg, na.rm = TRUE),
    cf = sum(event_type %in% corsi),
    .groups = "drop"
  ) |>
  left_join(
    toi_pp, by = "team"
  ) |>
  mutate(
    across(
      gf:cf,
      ~.x / toi_minutes * 60,
      .names = "{.col}_per_60"
    )
  ) |>
  mutate(
    across(
      .cols = ends_with("per_60"),
      ~scales::rescale(.x, c(0,1), c(min(.x), max(.x))),
      .names = "{.col}_scaled"
    )
  )

pp <- pp |>
  select(team,ends_with("per_60")) |>
  pivot_longer(
    cols = ends_with("per_60"), names_to = "stat", values_to = "real_value"
  ) |>
  left_join(
    pp |>
      select(team, ends_with("scaled")) |>
      rename_with(
        .cols = ends_with("scaled"),
        ~str_remove(.x, "_scaled")
      ) |>
      pivot_longer(
        cols = ends_with("per_60"), names_to = "stat", values_to = "scaled_value"
      ),
    by = c("team","stat")
  ) |>
  mutate(
    real_value = as.character(round(real_value, 1)),
    stat = factor(
      stat,
      levels = c("cf_per_60","xgf_per_60","gf_per_60")
    )
  ) |>
  left_join(
    hockeyR::team_logos_colors |>
      filter(status == "active") |>
      mutate(
        full_team_name = ifelse(team_abbr == "MTL", "Montréal Canadiens", full_team_name)
      ),
    by = c("team" = "full_team_name")
  )


pk <- pbp |>
  mutate(pk_team = case_when(
    home_skaters < away_skaters ~ home_name,
    away_skaters < home_skaters ~ away_name,
    home_skaters == away_skaters ~ NA_character_
  )) |>
  filter(strength_state %in% c("5v4","4v5","5v3","3v5","4v3","3v4")) |>
  group_by(team = pk_team) |>
  summarize(
    ga = sum(event_type == "GOAL"),
    xga = sum(xg, na.rm = TRUE),
    ca = sum(event_type %in% corsi),
    .groups = "drop"
  ) |>
  left_join(
    toi_pk, by = "team"
  ) |>
  mutate(
    across(
      ga:ca,
      ~.x / toi_minutes * 60,
      .names = "{.col}_per_60"
    )
  ) |>
  mutate(
    across(
      .cols = ends_with("per_60"),
      ~scales::rescale(.x, c(0,1), c(max(.x), min(.x))),
      .names = "{.col}_scaled"
    )
  )

pk <- pk |>
  select(team,ends_with("per_60")) |>
  pivot_longer(
    cols = ends_with("per_60"), names_to = "stat", values_to = "real_value"
  ) |>
  left_join(
    pk |>
      select(team, ends_with("scaled")) |>
      rename_with(
        .cols = ends_with("scaled"),
        ~str_remove(.x, "_scaled")
      ) |>
      pivot_longer(
        cols = ends_with("per_60"), names_to = "stat", values_to = "scaled_value"
      ),
    by = c("team","stat")
  ) |>
  mutate(
    real_value = as.character(round(real_value, 1)),
    stat = factor(
      stat,
      levels = c("ca_per_60","xga_per_60","ga_per_60")
    )
  ) |>
  left_join(
    hockeyR::team_logos_colors |>
      filter(status == "active") |>
      mutate(
        full_team_name = ifelse(team_abbr == "MTL", "Montréal Canadiens", full_team_name)
      ),
    by = c("team" = "full_team_name")
  )

# define functions

# for leaving breaks as integers
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

make_team_card <- function(tm){
  
  tm_colors <- filter(hockeyR::team_logos_colors |>
                      filter(status == "active") |>
                      mutate(
                        full_team_name = ifelse(team_abbr == "MTL", "Montréal Canadiens", full_team_name)
                      ), team_abbr == tm)
  
  tm_name <- pull(tm_colors, full_team_name)
  
  p_logo <- tm_colors |>
    ggplot(aes(0,0)) +
    ggimage::geom_image(
      aes(image = team_logo_espn),
      asp = 12/7, size = .55
      ) +
    theme_void()
  
  p_title <- tm_colors |>
    ggplot(aes(0,0)) +
    geom_text(
      aes(label = full_team_name),
      size = 12
    ) +
    theme_void()
  
  team_info <- standings |>
    mutate(
      rank_ord = case_when(
        division_rank == 1 ~ "1st",
        division_rank == 2 ~ "2nd",
        division_rank == 3 ~ "3rd",
        division_rank > 3 ~ paste0(division_rank,"th")
        ),
      Record = as.character(glue::glue("{wins}-{losses}-{ot} ({rank_ord})")),
      `Points %` = as.character(sprintf("%.3f", round(points_percentage, 3)) )
      ) |>
    filter(team_name == tm_colors$full_team_name) |>
    select(
      Division = division_name,
      Record,
      `Points %`,
      Streak = streak_streak_code
    ) |>
    pivot_longer(
      cols = everything(), names_to = "Updated", values_to = paste(max(pbp$game_date))
    )
  
  charto <- filter(offense, team_abbr == tm)
  
  p_off <- charto |>
    ggplot(aes(scaled_value, stat)) +
    geom_col(
      aes(fill = team_color1, color = team_color2), size = 1
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    geom_text(
      aes(label = real_value, x = scaled_value + .01),
      hjust = 0
    ) +
    scale_x_continuous(
      limits = c(0,1.08),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = c("Unblocked SH%","Unblocked Rate","Shot Attempts","Expected Goals","Goals")
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = NULL, y = NULL,
      #title = paste0(unique(charto$team)),
      subtitle = "5v5 Offense per 60 minutes"
    )
  
  chartd <- filter(defense, team_abbr == tm)
  
  p_def <- chartd |>
    ggplot(aes(scaled_value, stat)) +
    geom_col(
      aes(fill = team_color1, color = team_color2), size = 1
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    geom_text(
      aes(label = real_value, x = scaled_value + .01),
      hjust = 0
    ) +
    scale_x_continuous(
      limits = c(0,1.08),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = c("GSAx","Blocked Rate","Shot Attempts","Expected Goals","Goals")
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = NULL, y = NULL,
      #title = paste0(unique(chartd$team)),
      subtitle = "5v5 Defense per 60 minutes"
    )
  
  nz <- filter(nz_stats, team_abbr == tm)
  
  p_nz <- nz |>
    ggplot(aes(scaled_value, stat)) +
    geom_col(
      aes(fill = team_color1, color = team_color2), size = .75
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(
      limits = c(0,1.08),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = c("Defense","Offense","Total")
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = "Percentile Rank", y = NULL,
      #title = paste0(unique(chartd$team)),
      subtitle = "5v5 Neutral Zone Aggressiveness"
    )
  
  p_pp <- pp |>
    filter(team == tm_name) |>
    ggplot(aes(scaled_value, stat)) +
    geom_col(
      aes(fill = team_color1, color = team_color2), size = .75
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    geom_text(
      aes(label = real_value, x = scaled_value + .01),
      hjust = 0
    ) +
    scale_x_continuous(
      limits = c(0,1.08),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = c("Shot Attempts","Expected Goals","Goals")
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = "Percentile Rank", y = NULL,
      #title = paste0(unique(charto$team)),
      subtitle = "PP Offense per 60 minutes"
    )
  
  p_pk <- pk |>
    filter(team == tm_name) |>
    ggplot(aes(scaled_value, stat)) +
    geom_col(
      aes(fill = team_color1, color = team_color2), size = .75
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    geom_text(
      aes(label = real_value, x = scaled_value + .01),
      hjust = 0
    ) +
    scale_x_continuous(
      limits = c(0,1.08),
      expand = c(0,0),
      breaks = seq(0,1,.25),
      labels = paste0(seq(0,100,25),"%")
    ) +
    scale_y_discrete(
      labels = c("Shot Attempts","Expected Goals","Goals")
    ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = "Percentile Rank", y = NULL,
      #title = paste0(unique(chartd$team)),
      subtitle = "PK Defense per 60 minutes"
    )
  
  games <- pbp |>
    filter(home_abbreviation == tm | away_abbreviation == tm) |>
    filter(!is.na(event_team) & !is.na(event_team_abbr)) |>
    group_by(game_date, team = event_team, team_abbr = event_team_abbr) |>
    summarize(
      #toi = max(game_seconds) / 60,
      gf = sum(event_type == "GOAL"),
      xgf = sum(xg, na.rm = TRUE),
      .groups = "drop"
    )
  
  team_by_game <- games |>
    filter(team_abbr == tm) |>
    left_join(
      games |>
        filter(team_abbr != tm) |>
        rename(ga = gf, xga = xgf, opponent = team, opp_abbr = team_abbr),
      by = "game_date"
    ) |>
    mutate(
      gd = gf - ga,
      xgd = xgf - xga,
      roll_gd = zoo::rollsum(gd, k = 10, align = "right", fill = NA),
      roll_xgd = zoo::rollsum(xgd, k = 10, align = "right", fill = NA)
    ) |>
    pivot_longer(
      cols = starts_with("roll"),
      names_to = "stat", values_to = "value"
    ) |>
    mutate(
      stat = ifelse(stat == "roll_gd", "Observed","Expected")
    )
  
  team_colors <- tibble(color = c(
    filter(hockeyR::team_logos_colors, team_abbr == tm)$team_color1,
    filter(hockeyR::team_logos_colors, team_abbr == tm)$team_color2
  )) |>
    mutate(
      color = ifelse(color == "#FFFFFF","lightgray", color)
    )
  
  last10 <- team_by_game |>
    filter(!is.na(value)) |>
    ggplot(aes(game_date, value, color = stat)) +
    geom_hline(
      yintercept = 0, color = "gray"
    ) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = team_colors$color) +
    scale_y_continuous(
      breaks = integer_breaks()
    ) +
    theme_bw() +
    theme(
      legend.position = c(.25,1.08),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = "transparent"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      x = NULL, y = NULL, color = NULL,
      subtitle = "10-game rolling goal differentials\n"
    )
  
  card <- (p_logo + p_title) /
    (p_off + p_def + (last10 / p_nz)) +
    plot_layout(
      design = "
      ABB###
      CCDDEE
      CCDDFF
      "
    )
    #plot_annotation(
    #  title = tm_name,
    #  theme = theme(plot.title = element_text(hjust = .5, size = 24))
    #)
  card <- p_logo + p_title + p_off + p_def +
    gridExtra::tableGrob(team_info, rows = NULL) +
    last10 + p_pp + p_pk + p_nz +
    plot_layout(
      design = "
      ABBBEE
      CCDDEE
      CCDDFF
      CCDDFF
      GGHHII
      GGHHII
      "
    ) +
    plot_annotation(
      caption = "Team Stat Cards by @danmorse_ · data from hockeyR"
    )
  
  return(card)
  
}

for(i in unique(offense$team_abbr)){
  ggsave(
    glue::glue("team_cards/{tolower(i)}.png"),
    plot = make_team_card(i),
    width = 12, height = 7, dpi = 500
  )
}

