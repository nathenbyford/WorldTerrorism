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

states_ls <- as.list(us_states_df$state)

states <- matrix(nrow = 2, ncol = length(states_ls)+1)
states <- data.frame(states)
states[1, 1] <- 'State'
states[2, 1] <- 'Number of Attacks'

for (i in 1:51) {
  states[1, i + 1] <- as.character(states_ls[i]) 
  states[2, i + 1] <- sum(with(datus_s, provstate == as.character(states_ls[i])))
}

us.states <- us_states %>% arrange(NAME) %>% select(NAME)


tm_shape(us_states) + tm_fill(col = 'median_income_10') + tm_borders()
