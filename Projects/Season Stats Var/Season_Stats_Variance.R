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


# Get Basketball Reference team name info 
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


df_2021_gamelogs <- game_logs(seasons = 2021, result_types = "player")
