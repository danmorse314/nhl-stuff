## NHL Helper functions & datasets

This is the (future) home of any helper functions I use when analyzing NHL data. This is NOT the home of an NHL play-by-play scraper. The best one of those I'm aware of is from [Evolving Hockey](https://github.com/mhbw/evolving-hockey/blob/master/ALLSCRAPE.R).

The [data](https://github.com/danmorse314/nhl-stuff/tree/main/data) folder is for miscellaneous data files, the first of which is nhl_team_info.csv, which contains all 32 NHL team names, abbreviations, colors, and logos. It should make it easier to get proper colors and logos on any NHL-related plots.

Here's an example of how it might work:

```{r nhl team info example}
library(tidyverse)
# reading in the data
team_info <- read_csv("https://github.com/danmorse314/nhl-stuff/raw/main/data/nhl_team_info.csv",
                      col_types = cols())

# create a dummy df to make a bar chart
df <- tibble(
  value = runif(32, 15, 30)
) |>
  bind_cols(team_info)

df |>
  ggplot((aes(full_team_name, value))) +
  geom_col(fill = df$team_color1, color = df$team_color2, size = 1, width = .7) +
  ggimage::geom_image(
    aes(y = value + 1, image = team_logo_espn),
    size = .05, asp = 1.5
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = NULL, y = NULL)
```
![plot depicting all 32 nhl teams with primary and secondary colors along with primary logos](https://github.com/danmorse314/nhl-stuff/blob/main/figures/color%20plot%20example.png)

Alternate logos & color schemes are also included! The `team_logo_alternate` will match up best with the `team_color_alt1` & `team_color_alt2`. These are a somewhat random assortment of throwback logos & secondary logos, and in some cases throwback secondary logos.

```{r alternate color logo plot}
# reshuffle the random y variable just for fun
df <- tibble(
  value = runif(32, 15, 30)
) |>
  bind_cols(team_info)

df |>
  ggplot((aes(full_team_name, value))) +
  geom_col(fill = df$team_color_alt1, color = df$team_color_alt2, size = 1, width = .7) +
  ggimage::geom_image(
    aes(y = value + 1, image = team_logo_alternate),
    size = .05, asp = 1.5
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = NULL, y = NULL)
```
![plot depicting all 32 nhl teams with alternate logos and matching color schemes](https://github.com/danmorse314/nhl-stuff/blob/main/figures/color%20plot%20example%20alternate.png)

If you want to mix it up and make plots with some primary logos and some alternate logos, here's a quick snippet of code that'll do the trick:

```{r mixed logos}
# reshuffle
df <- tibble(
  value = runif(32, 15, 30)
) |>
  bind_cols(team_info)

# define teams to use alternate logos for
alt_teams <- c("ANA", "ARI", "CBJ", "LAK", "NJD")

# define which colors and logos for each team
df <- df |>
  mutate(
    color1 = ifelse(team_abbr %in% alt_teams, team_color_alt1, team_color1),
    color2 = ifelse(team_abbr %in% alt_teams, team_color_alt2, team_color2),
    logo = ifelse(team_abbr %in% alt_teams, team_logo_alternate, team_logo_espn)
  )

df |>
  ggplot((aes(full_team_name, value))) +
  geom_col(fill = df$color1, color = df$color2, size = 1, width = .7) +
  ggimage::geom_image(
    aes(y = value + 1, image = logo),
    size = .05, asp = 1.5
  ) +
  geom_text(
    aes(y = 1, label = full_team_name),
    color = "white", angle = 90, hjust = 0, vjust = .3
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)
```
![plot depicting all 32 nhl teams with alternate logos and matching color schemes](https://github.com/danmorse314/nhl-stuff/blob/main/figures/color%20plot%20example%20mixed.png)

More to come soon!
