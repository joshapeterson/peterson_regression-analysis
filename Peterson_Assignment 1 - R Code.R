# Libraries --------------------------------------------------------------------

library(tidyverse)
library(caret)
library(e1071)
library(car)
library(pROC)
library(Hmisc)
library(rpart)
library(rpart.plot)

## Data Prep -------------------------------------------------------------------

# Read in data

dfJP <- read.csv('data/inq2019.csv', na.strings = c(''))

# Initial exploration

summary(dfJP)
str(dfJP)

# Impute values

dfJP$TERRITORY <- with(dfJP, impute(TERRITORY, '2'))

# Transform skewed values to a normal distribution

dfJP$distance <- log10(dfJP$distance + 1)

# Change data type of dependent variable

dfJP$Enroll <- factor(dfJP$Enroll)

# Change necessary columns to factors

dfJP$sex <- factor(dfJP$sex)
dfJP$mailq <- factor(dfJP$mailq)
dfJP$telecq <- factor(dfJP$telecq)
dfJP$premiere <- factor(dfJP$premiere)
dfJP$stucar <- factor(dfJP$stucar)
dfJP$Instate <- factor(dfJP$Instate)
dfJP$ETHNICITY <- factor(dfJP$ETHNICITY)
dfJP$TERRITORY <- factor(dfJP$TERRITORY)

str(dfJP)

# Remove columns that will not be used in either model 

dfJP <- subset(dfJP, select = -c(ETHNICITY, sex, LEVEL_YEAR, ACADEMIC_INTEREST_1, ACADEMIC_INTEREST_2, IRSCHOOL, CONTACT_CODE1, CONTACT_DATE))

str(dfJP)

# Testing for multicollinearity

vif(glm(formula = Enroll~., family = binomial(link = 'logit'), data = dfJP))

# Create data partition

set.seed(100)
trainIndex <- createDataPartition(dfJP$Enroll,
                                  p = 0.7,
                                  list = FALSE,
                                  times = 1)

# Training data

dfJP.train <- dfJP[trainIndex,]

# Validation data

dfJP.valid <- dfJP[-trainIndex,]
dfJP.valid.nomissing <- na.omit(dfJP.valid)

## Regression Model ------------------------------------------------------------

regressionJP <- train(Enroll~.,
                     data = dfJP.train,
                     method = 'glm',
                     family = 'binomial',
                     na.action = na.pass)

summary(regressionJP)

# Evaluation of base regression model

# 1st evaluation - confusion matrix

prediction.regJP <- predict(regressionJP, newdata = dfJP.valid.nomissing)

confusionMatrix(prediction.regJP, dfJP.valid.nomissing$Enroll)

# 2nd evaluation - ROC and AUC

pred.probabilities.regJP <- predict(regressionJP, newdata = dfJP.valid.nomissing, type = 'prob')

regression.rocJP <- roc(predictor = pred.probabilities.regJP$`1`,
                        response = dfJP.valid.nomissing$Enroll,
                        levels = levels(dfJP.valid.nomissing$Enroll))

plot(regression.rocJP)
regression.rocJP$auc

## Decision Tree Model ---------------------------------------------------------

treeJP <- train(Enroll~.,
                data = dfJP.train,
                method = 'rpart',
                na.action = na.pass)

treeJP

prp(treeJP$finalModel, type = 2, extra = 106)

# 1st evaluation - confusion matrix

prediction.treeJP <- predict(treeJP, newdata = dfJP.valid.nomissing)
confusionMatrix(prediction.treeJP, dfJP.valid.nomissing$Enroll)

# 2nd evaluation - ROC and AUC

pred.probabilities.treeJP <- predict(treeJP, newdata = dfJP.valid.nomissing, type = 'prob')

tree.rocJP <- roc(predictor = pred.probabilities.treeJP$`1`,
                  response = dfJP.valid.nomissing$Enroll,
                  levels = levels(dfJP.valid.nomissing$Enroll))

plot(tree.rocJP)
tree.rocJP$auc
