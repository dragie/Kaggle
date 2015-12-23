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
stopCluster(cl)

library(h2o)
library(data.table)
library(dplyr)
h2o.server <- h2o.init( nthreads= -1)
train.hex <- as.h2o(train)
features=names(train)[!names(train) %in% c("Sales")]

gbmF_model_1 = h2o.gbm( x=features,
                        y = "Sales",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Sales",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)


dl_model_1 = h2o.deeplearning( x=features,
                               y = "Sales",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                      y = "Sales",
                      training_frame =train.hex ,
                      classification = 0)



rf_model1 <- h2o.randomForest( x=features,
                               y = "Sales",
                               training_frame =train.hex )
test_dl_model_2 = as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )

MySubmission = test[, c("Id"), with = FALSE]
test.hex  = as.h2o(test)

#Making the predictions
testPurchase_gbm_1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
testPurchase_gbm_2 = as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )

testPurchase_dl_model_1 = as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
testPurchase_dl_model_2 = as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
testPurchase_dl_model_3 = as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )

testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<0,0,testPurchase_gbm_1$predict)
testPurchase_gbm_2$predict=ifelse(testPurchase_gbm_2$predict<0,0,testPurchase_gbm_2$predict)

testPurchase_dl_model_1$predict=ifelse(testPurchase_dl_model_1$predict<0,0,testPurchase_dl_model_1$predict)
testPurchase_dl_model_2$predict=ifelse(testPurchase_dl_model_2$predict<0,0,testPurchase_dl_model_2$predict)
testPurchase_dl_model_3$predict=ifelse(testPurchase_dl_model_3$predict<0,0,testPurchase_dl_model_3$predict)

final=0.3*(testPurchase_dl_model_1$predict)+
  0.15*(testPurchase_dl_model_2$predict)+
  0.25*(testPurchase_dl_model_3$predict)+
  0.1*(testPurchase_gbm_1$predict)+
  0.2*(testPurchase_gbm_2$predict)



#Final Submission
MySubmission = final


write.csv(test_dl_model_2, "DL2.csv", row.names = F)



