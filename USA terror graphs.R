## Nathen byford
## STA 4371 Final project

rm(list = ls())
library(sf)
library(raster)
library(tidyverse)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet) 

data <- read_csv("~/Downloads/globalterrorismdb_0718dist.csv")

datus <- data %>% filter(country == 217)

pred <- datus %>% select(imonth, provstate, suicide, attacktype1, targtype1, 
                         weaptype1, propextent)

res <- datus %>% select(success)

datus_s <- cbind(res, pred)
datus_s <- na.omit(datus_s)

tm_shape(us_states) + tm_fill() + tm_borders()
