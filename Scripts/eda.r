library(tidyverse)
library(car)
library(ggally)
library(olsrr)

data <- read_csv("~/Downloads/globalterrorismdb_0718dist.csv")

datus <- data %>% filter(country == 217)

pred <- datus %>% select(imonth, provstate, suicide, attacktype1, targtype1, 
                         weaptype1, propextent)

res <- datus %>% select(success)

## Split the data 80% 20%




## make a full model