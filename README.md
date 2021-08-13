## NHL Helper functions & datasets

This is the home of any helper functions I use when analyzing NHL data. This is NOT the home of an NHL play-by-play scraper. The best one of those I'm aware of is from [Evolving Hockey](https://github.com/mhbw/evolving-hockey/blob/master/ALLSCRAPE.R)

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
  ggplot((aes(team_abbr, value))) +
  geom_col(fill = df$team_color1, color = df$team_color2, size = 1, width = .7) +
  ggimage::geom_image(
    aes(y = value + 1, image = team_logo_espn),
    size = .07, asp = 1.5
  ) +
  theme_bw() +
  labs(x = NULL, y = NULL)
```

Alternate logos & color schemes are also included! The `team_logo_alternate` will match up best with the `team_color_alt1` & `team_color_alt2`.

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

More to come soon!
