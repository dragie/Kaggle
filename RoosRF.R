# Load Dependeicies
library(caret)
library(randomForest)
library(readr)
library(lubridate)
library(plyr)
#library(doMC)
#registerDoMC(cores = 6)
# parallel   ---------------------------------------------------------
library(foreach)
library(doParallel)

#setup parallel back end to use 8 processors
cl<-makeCluster(6)
registerDoParallel(cl)

setwd("D:\\Kaggle\\Rossmann Store Sales\\")

# Set seed
set.seed(1338)
train <- read.csv("train.csv", stringsAsFactors = F)
test  <- read.csv("test.csv", stringsAsFactors = F)
store <- read.csv("store.csv", stringsAsFactors = F)

# Merge Store stuff
train <- merge(train, store, by=c('Store'))
test <- merge(test, store, by=c('Store'))
test <- arrange(test,Id)

# Only evaludated on Open days
train <- train[ which(train$Open=='1'),]
train$Open <- NULL
test$Open <- NULL

# Set missing data to 0
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# Date stuff
train$Date <- ymd(train$Date)
test$Date <- ymd(test$Date)
train$day <- as.factor(day(train$Date))
test$day <- as.factor(day(test$Date))
train$month <- as.factor(month(train$Date))
test$month <- as.factor(month(test$Date))
train$year <- as.factor(year(train$Date))
test$year <- as.factor(year(test$Date))
train$Date <- NULL
test$Date <- NULL

# always 1 for training and test
train$StateHoliday <- NULL
test$StateHoliday <- NULL

#Factorize stuff
train$DayOfWeek <- as.factor(train$DayOfWeek)
test$DayOfWeek <- as.factor(test$DayOfWeek)
train$Promo <- as.factor(train$Promo)
test$Promo <- as.factor(test$Promo)
train$SchoolHoliday <- as.factor(train$SchoolHoliday)
test$SchoolHoliday <- as.factor(test$SchoolHoliday)

# Factorize store stuff
train$StoreType <- as.factor(train$StoreType)
test$StoreType <- as.factor(test$StoreType)
train$Assortment <- as.factor(train$Assortment)
test$Assortment <- as.factor(test$Assortment)
train$CompetitionDistance <- as.numeric(train$CompetitionDistance)
test$CompetitionDistance <- as.numeric(test$CompetitionDistance)
train$Promo2 <- as.factor(train$Promo2)
test$Promo2 <- as.factor(test$Promo2)
train$PromoInterval <- as.factor(train$PromoInterval)
test$PromoInterval <- as.factor(test$PromoInterval)

competition_start <- strptime('20.10.2015', format='%d.%m.%Y')
train$CompetitionDaysOpen <- as.numeric(difftime(competition_start,
                                                 strptime(paste('1',
                                                                train$CompetitionOpenSinceMonth,
                                                                train$CompetitionOpenSinceYear, sep = '.'),
                                                          format='%d.%m.%Y'), units='days'))
test$CompetitionDaysOpen <- as.numeric(difftime(competition_start,
                                                strptime(paste('1',
                                                               test$CompetitionOpenSinceMonth,
                                                               test$CompetitionOpenSinceYear, sep = '.'),
                                                         format='%d.%m.%Y'), units='days'))
train$CompetitionDaysOpen[is.na(train$CompetitionDaysOpen)] <- 0
test$CompetitionDaysOpen[is.na(test$CompetitionDaysOpen)] <- 0

train$CompetitionWeeksOpen <- train$CompetitionDaysOpen/7
test$CompetitionWeeksOpen <- test$CompetitionDaysOpen/7

train$CompetitionMonthsOpen <- train$CompetitionDaysOpen/30
test$CompetitionMonthsOpen <- test$CompetitionDaysOpen/30

train$CompetitionYearsOpen <- train$CompetitionWeeksOpen/52
test$CompetitionYearsOpen <- test$CompetitionWeeksOpen/52

train$CompetitionOpenSinceMonth <- NULL
train$CompetitionOpenSinceYear <- NULL
test$CompetitionOpenSinceMonth <- NULL
test$CompetitionOpenSinceYear <- NULL

# target variables
train$Sales <- as.numeric(train$Sales)
train$Customers <- NULL #as.numeric(train$Customers)

train$Sales <- log(train$Sales+1)

set.seed(1338)
fitControl <- trainControl(method="cv", number=3, verboseIter=T)
rfFit <- train(Sales ~.,
               method="rf", data=train, ntree=50, importance=TRUE,
               sampsize=100000,
               do.trace=10, trControl=fitControl)

length_divisor <- 20
predictions<-foreach(m=1:400,.combine=cbind) %dopar% { 
  # using sample function without seed
  sampleRows <- sample(nrow(train), size=floor((nrow(train)/length_divisor)))
  rfFit <- train(Sales ~.,
                 method="rf", data=train[sampleRows,], ntree=50, importance=TRUE,
                 sampsize=100000,
                 do.trace=10, trControl=fitControl)
  #fit <- lm(readmitted ~ ., data = traindf[sampleRows,])
  predictions <- data.frame(pred <- predict(rfFit, test))
} 
stopCluster(cl)


pred <- predict(rfFit, test)
submit = data.frame(Id = test$Id, Sales = (exp(pred) -1))
stopCluster(cl)
write.csv(submit, "../submissions/rf_better_features.csv", row.names = FALSE, quote = FALSE)
