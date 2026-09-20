##### Created by Timothy Li #####
##### Created on: 2026-09-15 #####

##### Load Libraries #####
library(palmerpenguins)
library(tidyverse)
library(here)


### Load Data ###
# The data is part of the package and is called penguins 
glimpse(penguins)

### Filter Penguins by year and body mass

penguins_2008 <- filter(penguins, year == 2008)
penguins_5000 <- filter(penguins, body_mass_g > 5000)

penguins_89 <- filter(penguins, year == 2008 | year == 2009)
no_dream_penguins <- filter(penguins, island != "Dream")
penguins_ag <- filter(penguins, species %in% c("Adelie", "Gentoo"))


flipbm <- mutate(penguins, new_mass = flipper_length_mm + body_mass_g)

penguin_size <- mutate(penguins, size = if_else(body_mass_g > 4000, "big", "small"))




