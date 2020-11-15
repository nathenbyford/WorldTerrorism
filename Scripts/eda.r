library(tidyverse)
library(car)
library(ggally)
library(olsrr)
library(carets)
library(leaps)

data <- read_csv("~/Downloads/globalterrorismdb_0718dist.csv")

datus <- data %>% filter(country == 217)

pred <- datus %>% select(imonth, provstate, suicide, attacktype1, targtype1, 
                         weaptype1, propextent)

res <- datus %>% select(success)

datus_s <- cbind(res, pred)

## Split the data 80% 20%

set.seed(2)
sample <- sample.int(n = nrow(datus_s), size = floor(.80*nrow(datus_s)), 
                     replace = FALSE)

train <- datus_s[sample, ]
test <- datus_s[-sample, ]


## make a full model

full <- glm(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
              factor(weaptype1)+factor(propextent), data = datus_s)

summary(full)

par(mfrow = c(2, 2))
plot(full)

par(mfrow = c(1, 1))

## Use lasso for model selection

