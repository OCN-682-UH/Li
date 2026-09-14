##### This is my intro to plotting assignment #####
##### Created by Timothy Li #####
##### Created on 2026-09-13 #####
###################################################


##### Load Libraries #####
library(tidyverse)
library(here)
library(palmerspenguins)


##### Read in my data #####

# Installed from the palmerspenguin package
penguins


##### Clean up the data #####

# Eliminating NA values for my Boxplot
penguins_clean <- penguins[!is.na(penguins$sex), ]


##### Plotting the data #####
# The colors chosen are from Okabe-Ito Palette --> Used for colorblind-safe data visualization

plot1 <- ggplot(data = penguins_clean,
       mapping = aes(x = sex, 
                     y = flipper_length_mm,
                     color = sex)) + 
  geom_boxplot(linewidth = .5)+
  scale_color_manual(values = c("#CC79A7", "#56B4E9")) +
  facet_wrap(~ species) +
  labs(title = "Flipper Length by Sex Across Three Species",
       subtitle = "Comparing Adelie, Chinstrap, and Gentoo Penguins",
       x = "Sex", y = "Flipper length (mm)",
       caption = "Source: Palmer Station LTER / palmerspenguin package",
       color = "Sex"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = .5),
    axis.title.x = element_text(size = 12, hjust = .5, face = "bold"),
    axis.title.y = element_text(size = 12, hjust = .5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(face = "bold")
  )

plot1


##### Saving my Plot #####


ggsave(here("week_03", "output", "FlipperPenguin.png"), width = 8)






  

  


