setwd("D:\\Kaggle\\Prudential Life Insurance Assessment\\")

trainFull <- read.csv("train.csv")
testFull <- read.csv("test.csv")

dim(trainFull)
table(is.na(trainFull))

library(caret)
library(Metrics)
train <- trainFull
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
gbmFit1 <- train(as.factor(train$Response) ~ ., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)

str(train[,12])
train$Id <- NULL
table(is.na(train[,12]))
summary(train[,12])
is.na(train$Employment_Info_1) <- lapply( train$Employment_Info_1, median, na.rm = TRUE)
