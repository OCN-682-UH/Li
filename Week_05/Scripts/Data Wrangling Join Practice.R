##### Practice with Data Wrangling #####
##### Created by Timothy Li #####
##### Created on 2026-09-22 #####


# Load Libraries #####
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)


# Create a tibble #####

# Tibble 1
T1 <- tibble(
  Site.ID = c("A", "B", "C", "D"),
  Temperature = c(14.1, 16.7, 15.3, 12.8)
)

T1

# Tibble 2
T2 <- tibble(
  Site.ID = c("A", "B", "D", "E"),
  pH = c(7.3, 7.8, 8.1, 7.9)
)

T2

##### Left Join #####

left_join(T1, T2)
# Note that T1 has C and D while T2 has D and E

##### Right join #####




##### Join using by function #####

T4 <- tibble(
  Site.ID = c("A", "A", "B", "B"),
  Year = c(2020, 2021, 2020, 2021),
  Biomass = c(12.5, 15.3, 18.2, 16.9)
)

T5 <- tibble(
  SiteID = c("A", "A", "B"),
  Year = c(2020, 2021, 2021),
  Nutrients = c(8.2, 7.9, 9.1)
)

left_join(T4, T5, by = c("Site.ID" = "SiteID", "Year" = "Year"))


##### Handling naming conflicts #####

T6 <- tibble(
  Site.ID = c("A", "B", "C"),
  Notes = c("pristine", "degraded", "moderately impaired")
)

T7 <- tibble(
  Site.ID = c("A", "B", "D"),
  Notes = c("sunny", "shaded", "partially shaded"),
  Quality = c("good", "fair", "poor")
)

# Don't specify how to join — creates ambiguity with 'Notes'
left_join(T6, T7, by = "Site.ID")


##### Avoiding Naming conflicts #####

T6_renamed <- T6 |> 
  rename(Condition_Notes = Notes)

T7_renamed <- T7 |> 
  rename(Habitat_Notes = Notes)

left_join(T6_renamed, T7_renamed, by = "Site.ID")

##### Vector of Date Times #####

datetimes <- c(
  "02/24/2021 22:22:20",
  "02/25/2021 11:21:10",
  "02/26/2021 8:01:52"
)

datetimes

##### Convert the vector to datetime objects #####

datetimes <- mdy_hms(datetimes)

datetimes


##### Extract month as abbreviations ##### 

as.character(datetimes)
month(datetimes, label = TRUE)

##### Adding Time intervals #####

datetimes + hours(4)


##### Conddata #####

cond_data_2 <- read.csv("~/Repositories/Li/Week_05/Data/CondData.csv")

datatime_conddata <- cond_data_2 |>
  mutate(datetime = mdy_hms(date))


##### Practice #####

topt_data <- read.csv(here("Week_05", "Data", "topt_data.csv"))
site_characteristics_data <- read.csv(here("Week_05", "Data", "site.characteristics.data.csv"))

glimpse(topt_data)
glimpse(topt_data)

wide_characteristics_data <- site_characteristics_data |>
  pivot_wider(names_from = parameter.measured,
              values_from = values)

joined.files <- full.join(wide_topt_data, topt_data)

?pivot_longer

pivot_longer(cols      = Temp_in:percent_sgd, # select columns to pivot
             names_to  = "Variables",         # new column for old column names
             values_to = "Values") 
