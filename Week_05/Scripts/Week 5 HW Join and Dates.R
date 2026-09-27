##### This is my Join and Dates Assignment Looking at Salinity and Temperature across Depth Gradients #####
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
  mutate(date = with_tz(date, tzone = "US/Hawaii")) |> # Making sure time zone is HST
  mutate(date = round_date(date, "10 seconds")) |> # Rounding to 10 seconds to prep for join
  inner_join(depth_data) |>
  group_by(min_date = round_date(date, "minute")) |> # Round to minute for summaries 
    summarise(mean_date = mean(date, na.rm = TRUE),
              mean_depth = mean(Depth, na.rm = TRUE),
              "Avg Temperature" = mean(Temperature, na.rm = TRUE), # Column names to be Avg Temp / Salinity instead of mean_temp / mean_sal
              "Avg Salinity" = mean(Salinity, na.rm = TRUE)) |>    # I did this so facets will have these titles
  filter("Avg Salinity" >= 30) |>
  pivot_longer(cols = c("Avg Temperature", "Avg Salinity"), # Made it long data for easier graphing
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
  geom_smooth(method = "gam", linewidth = .5, color = "#D55E00", alpha = .3) + # Color friendly Red/Orange 
  facet_wrap(~ Measurement, nrow = 1, scales = "free_x") + # Free_x is used to give each graph their own X axis values (Individual scales)
  scale_fill_viridis_d() +
  scale_y_reverse() + # Makes more sense to have depth be descending (0 at top down by .1 meters) 
  scale_x_continuous() +
  labs(title = "Salinity and Temperature Profiles Across Depth Gradients",
      x = "Measurement Value", y = "Depth (m)",
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

##### Save the Plot ##### 

ggsave(here("Week_05", "Outputs", "HW5_Temp_Sal_Depth.png"), width = 10, height = 8)
  










