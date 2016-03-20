rm(list = ls())
library(readr)
library(xgboost)

setwd("D:\\Kaggle\\Airbnb Recruiting New User Bookings\\")
cat("reading the train and test data\n")
train <- read_csv("train_users.csv")
test  <- read_csv("test_users.csv")
age_gender_bkt <- read.csv("age_gender_bkts.csv")

dim(age_gender_bkt)
str(age_gender_bkt)
summary(age_gender_bkt)

library(ggplot2)
ggplot(data = age_gender_bkt, aes(x = country_destination, y = population_in_thousands)) + geom_bar()

str(train)

table(is.na(train$date_account_created))
train$month_account_created <- as.integer(format(train$date_account_created, "%m"))
train$year_account_created <- as.integer(format(train$date_account_created, "%y"))
train$day_account_created <- as.integer(format(train$date_account_created, "%d"))

table(is.na(train$date_first_booking))
train$month_account_created <- as.integer(format(train$date_account_created, "%m"))
train$year_account_created <- as.integer(format(train$date_account_created, "%y"))
train$day_account_created <- as.integer(format(train$date_account_created, "%d"))

