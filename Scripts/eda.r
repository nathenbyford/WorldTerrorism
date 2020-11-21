####################
## Nathen Byford  ##
## EDA 4371 Proj  ##
####################

rm(list = ls())
library(tidyverse)
library(car)
library(GGally)
library(glmnet)
library(caret)
library(leaps)
library(tree)

data <- read_csv("globalterrorismdb_0718dist.csv")

datus <- data %>% filter(country == 217)

pred <- datus %>% select(imonth, provstate, suicide, attacktype1, targtype1, 
                         weaptype1, propextent)

res <- datus %>% select(success)

datus_s <- cbind(res, pred)
datus_s <- na.omit(datus_s)

## Randomize the data points to preserve National Security
set.seed(253)

dat_us <- datus_s
dat_us <- dat_us %>% rename(., c(targtype = targtype1, 
                                 attacktype = attacktype1,
                                 weapontype = weaptype1))

attacknum_or <- c(unique(datus_s$attacktype1))
attacknum_new <- sample(c(1:length(attacknum_or)))

targnum_or <- c(unique(datus_s$targtype1))
targnum_new <- sample(c(1:length(targnum_or)))

weapnum_or <- c(unique(datus_s$weaptype1))
weapnum_new <- sample(c(1:length(weapnum_or)))

for (i in 1:8) {
  v <- attacknum_or[i]
  k <- attacknum_new[i]
  ind <- which(datus_s$attacktype1 == v)
  dat_us$attacktype1[ind] <- k  
}

for (i in 1:22) {
  v <- targnum_or[i]
  k <- targnum_new[i]
  ind <- which(datus_s$targtype1 == v)
  dat_us$targtype1[ind] <- k  
}

for (i in 1:10) {
  v <- weapnum_or[i]
  k <- weapnum_new[i]
  ind <- which(datus_s$weaptype1 == v)
  dat_us$weaptype1[ind] <- k  
}

rm(datus)
rm(datus_s)
rm(data)

## Split the data 80% 20%

set.seed(8)
sample <- sample.int(n = nrow(dat_us), size = floor(.80*nrow(dat_us)), 
                     replace = FALSE)

train <- dat_us[sample, ]
test <- dat_us[-sample, ]


## make a full model

full <- glm(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
              factor(weaptype1)+factor(propextent), family = "binomial", data = train)

summary(full)

par(mfrow = c(2, 2))
plot(full)

par(mfrow = c(1, 1))

## Step function to look for a better model

step_best <- step(full)
summary(step_best)

par(mfrow = c(2, 2))
plot(step_best)

par(mfrow = c(1, 1))

## Confussion matrix for stepwise model

step.pred <- predict(step_best, test, type = "response")
step.pred <- as.integer(as.logical(step.pred))
success <- test$success
table(step.pred, success)


## Use lasso for model selection

x <- model.matrix(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
                    factor(weaptype1)+factor(propextent), data = train)[, -1]
y <- train$success

x_test <- model.matrix(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
                         factor(weaptype1)+factor(propextent), data = test)[, -1]
x_test <- as.data.frame(x_test)
x_test <- transpose(x_test)
y_test <- test$success


cv.lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "default", keep = TRUE)

plot(cv.lasso)


coef(cv.lasso, cv.lasso$lambda.min)

coef(cv.lasso, cv.lasso$lambda.1se)

lambda_min <- cv.lasso$lambda.min

confusion.glmnet(cv.lasso$lambda.min, newx = x_test, newy = y_test, family = "binomial")

# Run trees models

train$success <- as.factor(train$success)
train$attacktype <- as.factor(train$attacktype)
train$imonth <- as.factor(train$imonth)
train$targtype <- as.factor(train$targtype)
train$weapontype <- as.factor(train$weapontype)
train$propextent <- as.factor(train$propextent)

model.tree <- tree(success ~ imonth+provstate+suicide+attacktype+targtype1+
                     weaptype1+propextent, data = train)

plot(model.tree)
text(model.tree, pretty = 1)

summary(model.tree)
