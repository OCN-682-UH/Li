
library(tidyverse)
library(here)

print(paste("The year is", 2000))

years <- c(2015:2021)

for (i in years) { # i is the index
  print(paste("The year is", i)) # loop over i
}

year_data <- tibble(year = rep(NA, length(years)),
                    year_name = rep(NA, length(years)))

year_data


testdata <- read_csv(here("Week_13", "Data", "cond_data", "011521_CT316_1pcal.csv"))

glimpse(testdata)

# List files in a directory 
# Point to the folder on the computer
CondPath <- here("Week_13", "Data", "cond_data")

# List all files in that path matching a pattern (here, anything with .csv)
# You can use regex to be more specific about filenames
files <- dir(path = CondPath, pattern = ".csv")

files

########

cond_data <- tibble(filename = rep(NA, length(files)),
                    mean_temp = rep(NA, length(files)),
                    mean_sal = rep(NA, length(files)))

cond_data

##########

raw_data <- read_csv(paste0(CondPath, "/", files[1]))
head(raw_data)

mean_temp <- mean(raw_data$Temperature, na.rm = TRUE)
mean_temp

mean_sal <- mean(raw_data$Salinity, na.rm = TRUE)
mean_sal

#######

for (i in seq_along(files)) {
  
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  glimpse(raw_data)
  
}

#######

for (i in seq_along(files)) {
  
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  
  cond_data$filename[i] <- files[i]
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE)
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE)
}

cond_data

########

set.seed(3817) # makes the random numbers reproducible
1:10 |> # the vector to iterate over
  map(rnorm, n = 15) |>
  map_dbl(mean)

set.seed(3817)
1:10 |>
  map(function(x) rnorm(15, x)) |> # make your own function
  map_dbl(mean)

set.seed(3817)
1:10 |>
  map(~ rnorm(15, .x)) |> # changes the arguments inside the function
  map_dbl(mean)





