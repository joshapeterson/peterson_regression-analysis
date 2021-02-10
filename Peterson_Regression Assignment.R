# Joshua Peterson
# Regression Assignment
# February 14, 2020

# Libraries

library(car)

# Data Prep

hour <- read.csv("hour.csv")
factor_cols <- c("season", "yr", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit")
factor_cols_alt <- c("season", "yr", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit")
str(hour)
attach(hour)

# 1. Data Exploration
# 1.a

summary(hour)
is.na(hour)
sum(is.na(hour))

## There are no missing values in the dataset

# 1.b

plot(cnt)
plot(log(cnt))

# 1.c

boxplot(hour$windspeed)
title("Boxplot of Windspeed", ylab = "Windspeed")
text(y = boxplot.stats(windspeed)$stats, labels = round(boxplot.stats(windspeed)$stats,2), x = 1.3)

## Outlier windspeeds are any values greater than 0.4627

# 1.d

pairs(hour[,c("temp", "atemp", "hum", "windspeed", "casual", "registered", "cnt")])




hour[,factor_cols] <- lapply(hour[,factor_cols], as.factor)

# 2.a

hour_train <- hour[1:10000,3:17]
hour_test <- hour[10001:17379,3:17]

# 2.b

mlr1 <- lm(cnt ~ ., data = hour_train)
summary(mlr1)

# 2.c

mlr2 <- lm(log(cnt) ~ . - workingday - casual - registered, data = hour_train)
summary(mlr2)

# 2.d

vif(mlr2)

# 2.e

mlr3 <- lm(log(cnt) ~ . - workingday - casual - registered - temp, data = hour_train)

# 2.f

summary(mlr3)
anova(mlr3)

# 2.g

summary(mlr3)

# 2.h

summary(mlr3)

# 3

summary(mlr3)

# 4

sum((mlr3$residuals)^2)/(10000-15-1)

hour_test$sqerr1 <- (log(hour_test$cnt) - predict(mlr3, hour_test))^2
mean(hour_test$sqerr1)


# 5


