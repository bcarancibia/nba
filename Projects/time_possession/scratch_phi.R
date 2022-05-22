library(nbastatR)
library(future)
library(tidyverse)
library(lubridate)

#### Regular Season
game_logs <- game_logs(seasons = 2022)

game_phi <- game_logs %>%
  filter(slugTeam == "PHI")

last_game <- play_by_play(game_ids = 22101228, nest_data = F, return_message = T)

last_game_v2 <- play_by_play_v2(game_ids = 22101228, nest_data = F, return_message = T)


#### Playoffs
Sys.setenv(VROOM_CONNECTION_SIZE=500072)
game_logs_playoffs <- game_logs(seasons = 2022,  season_types = "Playoffs")
game_phi_playoffs <- game_logs_playoffs %>%
  filter(slugTeam == "PHI")

last_game_v2_playoffs <- play_by_play_v2(game_ids = 42100206, nest_data = F, return_message = T)

df <- last_game_v2_playoffs %>%
  mutate(dtm = ms(timeQuarter),
         dtms = seconds(dtm),
         ptm = lag(dtms),
         elp = ptm-dtms)


p_df <- df %>%
  select(slugScore, idGame, numberEventMessageType, numberPeriod, timeQuarter,
         descriptionPlayHome, descriptionPlayNeutral, descriptionPlayVisitor,
         elp) %>%
  filter(numberEventMessageType == 1 | numberEventMessageType == 2) %>%
  filter(!is.na(descriptionPlayHome)) %>%
  filter(elp <= 24)

#started plotting add theme and make it density
ggplot(p_df, aes(elp)) + 
  geom_bar()



  

#this below doesn't really work yet
#na_remove <- last_game_v2_playoffs %>%
  #filter(!is.na(descriptionPlayHome))

#time_test <- na_remove %>%
  #unite("time_left", minuteRemainingQuarter:secondsRemainingQuarter, sep = ":", remove = FALSE )

#last_game_v2_playoffs$timeQuarter <- ms(last_game_v2_playoffs$timeQuarter)


#last_game_v2_playoffs <- last_game_v2_playoffs %>%
  #mutate(time_interval = timeQuarter - lag(timeQuarter))
  



