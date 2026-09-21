### Today we are going to practice tidyr with biogeochemistry data from Hawaii ####
### Created by: Timothy Li #############
### Updated on: 2026-09-20 ####################

#### Load Libraries ######
library(tidyverse)
library(here)


### Load data ######
ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
glimpse(ChemData)


##### Separate Tide and Time #####

ChemData_clean <- ChemData |>
  drop_na() |>
  separate_wider_delim(cols  = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time")) |>
  mutate(Site_Zone = paste(Site, Zone, sep = "."))

head(ChemData_clean)

# If we wanted to keep original tide_time columns, would put cols_remove = FALSE 

##### Make long data frame #####

ChemData_long <- ChemData_clean |>
  pivot_longer(cols      = Temp_in:percent_sgd, # select columns to pivot
               names_to  = "Variables",         # new column for old column names
               values_to = "Values")            # new column for the values

##### Summary Statistics ##### 

ChemData_long |>
  group_by(Variables, Site) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE))


ChemData_long |>
  group_by(Site, Zone, Tide) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE))


##### Plot Example #####

ChemData_long |>
  ggplot(aes(x = Site, y = Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free")


##### Converting Back to Wide #####

ChemData_wide <- ChemData_long |>
  pivot_wider(names_from  = Variables,
              values_from = Values)


##### All together #####

ChemData_clean <- ChemData |>
  drop_na() |>
  separate_wider_delim(cols        = Tide_time,
                       delim       = "_",
                       names       = c("Tide", "Time"),
                       cols_remove = FALSE) |>
  pivot_longer(cols      = Temp_in:percent_sgd,
               names_to  = "Variables",
               values_to = "Values") |>
  group_by(Variables, Site, Time) |>
  summarise(mean_vals = mean(Values, na.rm = TRUE)) |>
  pivot_wider(names_from  = Variables,
              values_from = mean_vals) |>
  write_csv(here("Week_04", "Output", "summary.csv"))







