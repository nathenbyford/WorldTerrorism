####################
## Nathen Byford  ##
## EDA 4371 Proj  ##
####################

rm(list = ls())
## Load Packages
library(tidyverse)
library(car)
library(GGally)
library(glmnet)
library(caret)
library(leaps)
library(tree)
library(randomForest)

## Read data
data <- read_csv("./Data/globalterrorismdb_0718dist.csv")


## Clean data
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
dat_us <- na.omit(dat_us)

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

## Confusion Matrix for full model
table(as.numeric(as.logical(predict(full, test, type = "response"))), test$success)


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

## Make model matrix for lasso model function
x <- model.matrix(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
                    factor(weaptype1)+factor(propextent), data = train)[, -1]
y <- train$success

x_test <- model.matrix(success ~ imonth+provstate+suicide+factor(attacktype1)+factor(targtype1)+
                         factor(weaptype1)+factor(propextent), data = test)

y_test <- test$success

## Run lasso model
cv.lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "default", keep = TRUE)

plot(cv.lasso)
## Check coefficients
coef(cv.lasso, cv.lasso$lambda.min)

coef(cv.lasso, cv.lasso$lambda.1se)

lambda_min <- cv.lasso$lambda.min

# make glmnet function
lasso.model <- glmnet(x, y, alpha = 1,family = "binomial", lambda = cv.lasso$lambda.1se)

## Try to predict with new data
idmin = match(cv.lasso$lambda.min, cv.lasso$lambda)

confusion.glmnet(cv.lasso$fit.preval,newx = x_test, newy = y_test, family = "binomial")[[idmin]]
## Get error about the new x in confusion function


# Ridge regression Model(Doesn't help at all)
cv.ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0, type.measure = "mse", keep = TRUE)

plot(cv.ridge)
## Picks every single variable


# Run trees models

# Make as.factor and not numeric for model
train$attacktype <- as.factor(train$attacktype)
train$imonth <- as.factor(train$imonth)
train$targtype <- as.factor(train$targtype)
train$weapontype <- as.factor(train$weapontype)
train$propextent <- as.factor(train$propextent)

model.tree <- tree(success ~ imonth+provstate+suicide+attacktype+targtype1+
                     weaptype1+propextent, data = train)

plot(model.tree)
text(model.tree, pretty = 1)

tree_pred <- predict(model.tree, test, type = "respponse")

summary(model.tree)

table(tree_pred, y_test)

## Random Forest model
rf.model <- randomForest(success ~ imonth+provstate+suicide+attacktype+targtype1+
                           weaptype1+propextent, data = train)

rf.pred <- predict(rf.model, newdata = train)
table(rf.pred, y)
summary(rf.model)

varImpPlot(rf.model)

