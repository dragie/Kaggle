# Kaggle Bike Sharing Demand

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

test <- read.csv("D:\\Kaggle\\Bike Sharing Demand\\test.csv", header =TRUE, sep = ",")
train <- read.csv("D:\\Kaggle\\Bike Sharing Demand\\train.csv", header =TRUE, sep = ",")
test$casual <- 0
test$registered <- 0
test$count <- 0
data <- rbind(train,test)
str(data)
table(is.na(data))
par(mfrow = c(4,2))
par(mar = rep(2,4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)
round(prop.table(table(data$weather)), digits = 2) *100
prop.table(table(data$holiday))
prop.table(table(data$workingday))
weekends <- cbind(data$holiday, data$workingday)
table(weekends)
data$season <- as.factor(data$season)
data$weather <- as.factor(data$weather)
data$holiday <- as.factor(data$holiday)
data$workingday <- as.factor(data$workingday)

data$hour <- as.factor(substr(data$datetime, 12, 13))
train <- data[as.integer(substr(data$datetime,9,10))<20,]
test <- data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$hour, xlab = "hour", ylab = "count of users")
boxplot(train$casual~train$hour, xlab = "hour", ylab = "count of casual users")
boxplot(train$registered~train$hour, xlab = "hour", ylab = "count of registered users")
boxplot(log(train$count)~train$hour, xlab = "hour", ylab = "log(count of users)")


date <- substr(data$datetime, 1, 10)
days <- weekdays(as.Date(date))
boxplot(data$count~days, xlab = "days", ylab = "count of users")
boxplot(data$casual~days, xlab = "days", ylab = "count of casual users")
boxplot(data$registered~days, xlab = "days", ylab = "count of registered users")

boxplot(data$count~data$weather, xlab = "weather", ylab = "count of users")
boxplot(data$casual~data$weather, xlab = "weather", ylab = "count of casual users")
boxplot(data$registered~data$weather, xlab = "weather", ylab = "count of registered users")

sub <- data.frame(train$registered,train$casual,train$count,train$temp,
                 train$humidity,train$atemp,train$windspeed)
round(cor(sub), digits = 2)

data$year <- as.factor(substr(data$datetime,1,4))
train <- data[as.integer(substr(data$datetime,9,10))<20,]
test <- data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year, xlab = "year", ylab = "count")


train$hour <- as.integer(train$hour)
test$hour <- as.integer(test$hour)
d <- rpart(registered~hour, data = train)
fancyRpartPlot(d)

data <- rbind(train,test)
data$dp_reg <- 0
data$dp_reg[data$hour < 8] <- 1
data$dp_reg[data$hour >= 22] <- 2
data$dp_reg[data$hour > 9 & data$hour < 18] <- 3
data$dp_reg[data$hour == 8] <- 4
data$dp_reg[data$hour == 9] <- 5
data$dp_reg[data$hour == 20 | data$hour == 21] <- 6
data$dp_reg[data$hour == 19 | data$hour == 18] <- 7

temp_reg <- 0
data$temp_reg[data$hour < 8] <- 1
data$temp_reg[data$hour >= 22] <- 2
data$temp_reg[data$hour > 9 & data$hour < 18] <- 3
data$temp_reg[data$hour == 8] <- 4
data$temp_reg[data$hour == 9] <- 5
data$temp_reg[data$hour == 20 | data$hour == 21] <- 6
data$temp_reg[data$hour == 19 | data$hour == 18] <- 7

temp_cas <- 0
data$temp_cas[data$hour < 8] <- 1
data$temp_cas[data$hour >= 22] <- 2
data$temp_cas[data$hour > 9 & data$hour < 18] <- 3
data$temp_cas[data$hour == 8] <- 4
data$temp_cas[data$hour == 9] <- 5
data$temp_cas[data$hour == 20 | data$hour == 21] <- 6
data$temp_cas[data$hour == 19 | data$hour == 18] <- 7

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1

train$hour=as.factor(train$hour)
test$hour=as.factor(test$hour)
train$hour=as.factor(train$hour)
test$hour=as.factor(test$hour)


set.seed(2)
fit1 <- randomForest(log(registered+1) ~ hour +workingday+days+holiday+
                       day_type +temp_reg+humidity+atemp+windspeed+season+
                       weather+dp_reg+weekend+year+year_part, data=data,
                     importance=TRUE, ntree=250)
pred1=predict(fit1,test)