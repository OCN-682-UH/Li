##### This is my first script. I am learning how to import data #####
##### Created by Timothy Li #####
##### Created on 2026-09-07 #####
##################################################################

##### Load Libraries ##### 
library(tidyverse)
library(here)

###### Read in my data #####

weightdata <- read_csv(here("Week_02", "Data", "weightdata.csv"))

##### Data Analysis #####

head(weightdata)
tail(weightdata)
view(weightdata)





