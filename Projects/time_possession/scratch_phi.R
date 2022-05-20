library(nbastatR)
library(future)
library(tidyverse)

#### Regular Season
game_logs <- game_logs(seasons = 2022)

game_phi <- game_logs %>%
  filter(slugTeam == "PHI")

last_game <- play_by_play(game_ids = 22101228, nest_data = F, return_message = T)

last_game_v2 <- play_by_play_v2(game_ids = 22101228, nest_data = F, return_message = T)


#### Playoffs

game_logs_playoffs <- game_logs(seasons = 2022,  season_types = "Playoffs")
game_phi_playoffs <- game_logs_playoffs %>%
  filter(slugTeam == "PHI")
