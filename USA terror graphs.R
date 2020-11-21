## Nathen byford
## STA 4371 Final project

rm(list = ls())
library(tidyverse)
theme_set(theme_classic())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(usmap)

data <- read_csv("globalterrorismdb_0718dist.csv")

datus <- data %>% filter(country == 217)

pred <- datus %>% select(imonth, provstate, suicide, attacktype1, targtype1, 
                         weaptype1, propextent)

res <- datus %>% select(success)

datus_s <- cbind(res, pred)
datus_s <- na.omit(datus_s)

states_ls <- as.list(unique(datus_s$provstate))

states <- matrix(nrow = 2, ncol = length(states_ls))
states <- data.frame(states)
states[1, 1] <- 'State'
states[2, 1] <- 'Number of Attacks'

for (i in 1:51) {
  states[1, i + 1] <- as.character(states_ls[i]) 
  states[2, i + 1] <- as.numeric(sum(with(datus_s, provstate == as.character(states_ls[i]))))
}
usa <- us_map()

states.dat <- rbind(usa, states)

plot_usmap(regions = "states", values = states) +
  scale_fill_continuous(name = "Number of Terror Attacks")
