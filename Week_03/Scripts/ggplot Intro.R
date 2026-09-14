##### Created by Timothy Li #####
##### Created on 2026-10-9  #####



library(tidyverse)
library(palmerpenguins)

glimpse(penguins)
head(penguins)


ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) + 
        geom_point()+
        geom_smooth()+ 
          labs(title = "Bill depth and length",
               subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
               x = "Bill depth (mm)", y = "Bill length (mm)",
               ) 
    
     
 




