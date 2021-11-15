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
library(ggbeeswarm)
library(prismatic)
library(ggrepel)
library(paletteer)

#Microsoft Sans Serif

theme_ben <- function () {
  theme_minimal(base_size=12, base_family="Microsoft Sans Serif") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = #FFFFFF, color = #FFFFFF)
      ))
}


url <- 'https://cds-api.nj.betmgm.com/bettingoffer/fixtures?x-bwin-accessid=ZTllNjllODUtOWQwNS00YmU4LWE4NTEtZGZjOTkzMGM5OWU4&lang=en-us&country=US&userCountry=US&subdivision=US-Pennsylvania&fixtureTypes=Standard&state=Latest&offerMapping=Filtered&offerCategories=Outrights&fixtureCategories=Outrights&sportIds=7&regionIds=9&competitionIds=6004&skip=0&take=50&sortBy=Tags'

library(jsonlite)
# read in json file
x <- read_json(url)

# extract awards bets
x <- x[["fixtures"]][[4]]
x <- x[["games"]]

# MVP table
df_mvp <- bind_rows(x[[1]][['results']])

# Clean up MVP data
df_mvp <- df_mvp %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "MVP", 
         name = as.character(name))


# DPOY table
df_dpoy <- bind_rows(x[[2]][['results']])

df_dpoy <- df_dpoy %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "DPOY", 
         name = as.character(name))

# ROY table
df_roy <- bind_rows(x[[3]][['results']])

df_roy <- df_roy %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "ROY", 
         name = as.character(name))

# SMOY table
df_smoy <- bind_rows(x[[4]][['results']])

df_smoy <- df_smoy %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "SMOY", 
         name = as.character(name))

# COY table
df_coy <- bind_rows(x[[5]][['results']])

df_coy <- df_coy %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "COY", 
         name = as.character(name))


# MIP table
df_mip <- bind_rows(x[[6]][['results']])

df_mip <- df_mip %>% 
  group_by(id) %>% 
  slice(1:1) %>% 
  mutate(award = "MIP", 
         name = as.character(name))

# combine tables into one df
df <- bind_rows(df_mvp, df_dpoy, df_coy, df_roy, df_mip, df_smoy)

df$award <- as.factor(df$award)
df$award <- factor(df$award, levels = c("MVP", "ROY", "DPOY", "SMOY", "MIP", "COY"))
df <- df %>%
  filter(award == "MVP")


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
