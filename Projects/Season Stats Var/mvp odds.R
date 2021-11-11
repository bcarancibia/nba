#getting MVP Odds

library(tidyverse)
library(rvest)
library(extrafont)

p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text=element_text(family="Microsoft Sans Serif"))
p

library(showtext)
# You will need to have internet connection
# If you restart R you will need to execute this code again to use the font
font_add_google(name = "Roboto",   # Name of the font on the Google Fonts site
                family = "roboto") # Name you want to use to call the font
