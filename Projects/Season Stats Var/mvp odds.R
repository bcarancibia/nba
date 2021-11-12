#getting MVP Odds

library(tidyverse)
library(rvest)
library(extrafont)
library(ggbeeswarm)
library(prismatic)
library(ggrepel)
library(paletteer)
library(magick)

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



#make a plot of mvps

p <- df %>%
  ggplot(aes(x=americanOdds, y = fct_rev(award), label=name)) +
  geom_quasirandom(aes(fill = award, color = after_scale(clr_darken(fill, 0.3))), 
                   shape = 21, 
                   size = 2.5, 
                   alpha = 0.75, 
                   groupOnX = FALSE, 
                   width = 0.3)+
  geom_text_repel(aes(color = award), 
                  position = position_quasirandom(groupOnX = FALSE),
                  fontface = 'bold', 
                  family = "Microsoft Sans Serif", 
                  size = 1,
                  segment.size = .1, 
                  min.segment.length = .15,
                  box.padding = 0.05)  +
  scale_x_log10(limits = c(100, 50000)) + 
  scale_fill_manual(values = c("#00B8AAFF", 
                               "#FD625EFF", 
                               "#5F6B6DFF", 
                               "#8AD4EBFF", 
                               "#FE9666FF", 
                               "#A66999FF")) + 
  scale_color_manual(values = c("#00B8AAFF", 
                                "#FD625EFF", 
                                "#5F6B6DFF", 
                                "#8AD4EBFF", 
                                "#FE9666FF", 
                                "#A66999FF")) +
  labs(x = "Odds (logged)", 
       y = "", 
       title = "Odds To Win 2021-22 NBA Regular Season Awards",
       subtitle = paste0("As of ", format(Sys.Date(), "%b. %d, %Y")), 
       caption = "+1000 odds means a bet of $100 wins you $1000, plus the intial $100 wager") + 
  theme_ben() + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold'),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = 'plot',
        plot.margin = margin(10, 10, 15, 10), 
        legend.position = 'none') 

