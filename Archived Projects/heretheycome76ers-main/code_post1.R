library(nbastatR)
library(tidyverse)
library(future)
library(sfthemes)

import_inter()

plan(multisession) 

bref_players_stats(seasons = 2016:2021, tables = c("advanced", "totals"))



embiid_totals <- dataBREFPlayerTotals %>%
  filter(namePlayer == "Joel Embiid")  %>%
  filter(isSeasonCurrent == TRUE) 


embiid_advanced <- dataBREFPlayerAdvanced %>%
  filter(namePlayer == "Joel Embiid") %>%
  filter(isSeasonCurrent == TRUE)


lebron_totals <- dataBREFPlayerTotals %>%
  filter(namePlayer == "LeBron James")  %>%
  filter(isSeasonCurrent == TRUE)

lebron_advanced <- dataBREFPlayerAdvanced %>%
  filter(namePlayer == "LeBron James")  %>%
  filter(isSeasonCurrent == TRUE) 

jokic_totals <- dataBREFPlayerTotals %>%
  filter(namePlayer == "Nikola Jokic")%>%
  filter(isSeasonCurrent == TRUE)

jokic_advanced <- dataBREFPlayerAdvanced %>%
  filter(namePlayer == "Nikola Jokic")  %>%
  filter(isSeasonCurrent == TRUE) 


totals <- bind_rows(embiid_totals,lebron_totals,jokic_totals)
advanced <- bind_rows(embiid_advanced, lebron_advanced, jokic_advanced)


totals_filter <- totals %>% 
  select("namePlayer", "countGames", "pctFG", "pctFG3", "pctFT",
         "minutesTotals", "trbTotals", "astTotals", "ftmTotals",
         "stlTotals", "blkTotals")

total_advanced <- advanced %>%
  select("namePlayer", "countGames", "minutes", "ratioPER", "pctTrueShooting",
         "ratioOWS", "ratioDWS", "ratioWS", "ratioWSPer48", "ratioVORP")
  







