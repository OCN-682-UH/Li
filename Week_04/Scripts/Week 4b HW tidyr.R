### This is my tidyr assignment script looking at pH vs Salinity different Zones in Spring during Nightime Low Tides ####
### Created by: Timothy Li #############
### Updated on: 2026-09-20 ####################


#### Load Libraries ######
library(tidyverse)
library(here)


##### Read in my data #####

ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
glimpse(ChemData)


##### Clean the Data #####

ChemData_clean <- ChemData |>
  drop_na() |>            # Remove all NAs
  separate_wider_delim(cols  = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time")) |>     # Separating Tide/Time column
  filter(Season == "SPRING", Tide == "Low", Time == "Night")  # Filtering for observations in Spring, Low Tide, and Night 
  

##### Pivot into long format #####

ChemData_chem_long <- ChemData_clean |>
  pivot_longer(cols      = Temp_in:percent_sgd, 
               names_to  = "Variables",         
               values_to = "Values")   


##### Calculate summary statistics #####

ChemData_chem_long |>
  group_by(Variables, Zone) |>      # Summarizing for Variable and Zone
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE),
            Param_sd = sd(Values, na.rm = TRUE)) |>
  write_csv(here("Week_04", "Output", "HW4b_tidyr_summary.csv"))
  
  
##### Create Plot #####

pH_salinity_plot <- ggplot(data = ChemData_clean,
                            mapping = aes(x = pH,
                                          y = Salinity,
                                          fill = Zone)) +
  geom_point(size = 1.75) +
  facet_wrap(~ Zone, nrow = 3) +    # 3 rows, plots stacked on one another  
  geom_smooth(method = "lm", linewidth = .5, color = "#CC79A7") +     # Line color is colorblind friendly 
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(7.85, 8.20, by = .025)) +
  labs(title = "pH vs Salinity in the Spring during Nighttime Low Tides",
       subtitle = "Comparing the Ambient, Diffuse, and Transition Zones",
       x = "pH", y = "Salinity",
       caption = "Silbiger et al. 2020 Proceedings of the Royal Society: B"
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

pH_salinity_plot


##### Save the Plot #####

ggsave(here("week_04", "output", "HW4b_pHvsSalinity.png"), width = 10, height = 8)



