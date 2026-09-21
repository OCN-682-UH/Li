##### This is my code for the dplyr assignment looking at the Log Body Mass of Female penguins#####
##### Created by Timothy Li #####
##### Updated on: 2026-09-21 #####


##### Load Libraries #####

library(palmerpenguins)
library(tidyverse)
library(here)


### Load Data ###

# The data is part of the package and is called penguins 
glimpse(penguins)


##### Calculating the Mean and Variance of Body Mass by Species #####

mean_variance_penguin <- penguins |>
  drop_na(body_mass_g, species, island, sex) |> # Dropping NA in these columns
  group_by(species, island, sex) |>
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            var_body_mass = var(body_mass_g, na.rm = TRUE))

mean_variance_penguin


##### Calculate Female log Body Mass #####

female_penguins <- penguins |>
  drop_na(sex) |>
  filter(sex == "female") |>
  mutate(log_body_mass = log(body_mass_g)) |>  # Adding a log body mass column
  select(species, island, sex, log_body_mass)

female_penguins

  
##### Plotting the Data. #####


# Color filled by species 
# Expand function used to align bins directly on x axis. .1 space for top of graph
# Boundary and closed use to place bins between each x axis point (7.9 to 8.0, >)

plot_log_penguins <- ggplot(data = female_penguins,
                mapping = aes(x = log_body_mass,
                              fill = species)) + 
  geom_histogram(binwidth = .1, color = "black", boundary = 0, closed = "left") + 
  facet_wrap(~ species) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(breaks = seq(0, 40, by = 2), expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(breaks = seq(7, 9, by = .1)) +
  labs(title = "Distribution of Log Body Mass (g) Across Three Species",
       subtitle = "Comparing Adelie, Chinstrap, and Gentoo Penguins",
       x = "Log of Body Mass (g)", y = "Frequency of Occurrence",
       caption = "Source: Palmer Station LTER / palmerspenguin package"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = .5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

plot_log_penguins


##### Save the Plot #####

ggsave(here("week_04", "output", "HW4a_LogBodyMassPenguin.png"), width = 10)



