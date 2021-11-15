# Load packages
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(janitor)
library(hablar)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(gt)
library(stringr)



'''
Get Basketball Reference team name info 
bref_tms <- dictionary_bref_teams()

bref_tms <- bref_tms %>% 
  filter(seasonLast == 2022) %>% 
  select(nameTeamBREF, slugTeamBREF)

bref_tms <- bref_tms %>% 
  mutate(slugTeamBREF = case_when(
    slugTeamBREF == "NJN" ~ "BRK", 
    slugTeamBREF == "CHA" ~ "CHO", 
    slugTeamBREF == "NOH" ~ "NOP",
    TRUE ~ slugTeamBREF
  ))
df_advanced_stats <- NBAPerGameAdvStatistics(season = 2021)
df_advanced_stats_36 <- NBAPerGameStatisticsPer36Min(season = 2021)

'''

#NBA Stat R - grab season stats per game to try to show variance

df_2021_gamelogs <- game_logs(seasons = 2021, result_types = "player")
df_2022_gamelogs <- game_logs(seasons = 2022, result_types = "player")


selected_logs <- df_2022_gamelogs %>%
  select(namePlayer, numberGamePlayerSeason, idPlayer,
         fgm:pctFT, fg2m:fpts)

filter <- selected_logs %>%
  group_by(namePlayer) %>%
  summarise(minutes = sum(minutes))

player_logs <- selected_logs %>%
  filter(namePlayer %in% filter$namePlayer)

player_logs_separate <- player_logs %>%
  separate(namePlayer, c("Name", "Surname"))

df_separate <- df %>%
  separate(name, c("Name", "Surname"))

mvp_stats <- player_logs_separate %>%
  filter(Surname %in% df_separate$Surname)

mvp_stats$namePlayer <- str_c(mvp_stats$Name, ' ', mvp_stats$Surname)

target <- c("Stephen Curry", "Kevin Durant", "Giannis Antetokounmpo",
            "Luka Doncic", "Joel Embiid", "Nikola Jokic", "Jimmy Butler",
            "Paul George", "Ja Morant", "Donovan Mitchell")

mvp_stats_top10 <- mvp_stats %>%
  filter(namePlayer %in% target)

#not working maybe look at gt?
library(gtExtras)
plus_minus_sparkline <- mvp_stats_top10 %>%
  dplyr::group_by(namePlayer) %>%
  dplyr::summarise(plus_minus = list(plusminus), .groups = 'drop') %>%
  gt() %>%
  cols_label(
    namePlayer = "Player",
    plus_minus = "Plus Minus") %>%
  gt_sparkline(plus_minus)



