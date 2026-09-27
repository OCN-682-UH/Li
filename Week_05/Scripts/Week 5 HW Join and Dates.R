##### This is my Join and Dates Assignment  #####
##### Created by Timothy Li #####
##### Created on 2026-09-25 #####


# Load Libraries #####
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)


##### Load data ######
cond_data <- read_csv(here("Week_05", "Data", "CondData.csv"))
glimpse(cond_data)

depth_data <- read_csv(here("Week_05", "Data", "DepthData.csv"))
glimpse(depth_data)


##### Convert date columns #####

date_cond_data <- cond_data |>
  mutate(date = mdy_hms(date)) |>
  mutate(date = with_tz(date, tzone = "US/Hawaii")) |>
  mutate(date = round_date(date, "10 seconds")) |>
  inner_join(depth_data) |>
  group_by(min_date = round_date(date, "minute")) |>
    summarise(mean_date = mean(date, na.rm = TRUE),
              mean_depth = mean(Depth, na.rm = TRUE),
              mean_temp = mean(Temperature, na.rm = TRUE),
              mean_salinity = mean(Salinity, na.rm = TRUE)) |>
  filter(mean_salinity >= 30) |>
  pivot_longer(cols = c(mean_temp, mean_salinity),
               names_to = "Measurement",
               values_to = "Values")

##### Create Plot #####


depth_temp_sal_plot <- ggplot(data = date_cond_data,
                           mapping = aes(x = Values,
                                         y = mean_depth,
                                         fill = Measurement,
                                         shape = Measurement,
                                        )) +
  geom_point(size = 2, color = "black", alpha = .75) +
  geom_smooth(method = "gam", linewidth = .5, color = "#56B4E9", alpha = .5) +
  facet_wrap(~ Measurement, nrow = 1, scales = "free_x") +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_reverse() +
  scale_x_continuous() +
  labs(title = "Salinity and Temperature Profiles Across Depth Gradients",
      x = "Measurement Value", y = "Depth (m)",
      caption = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = .5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

depth_temp_sal_plot


  










