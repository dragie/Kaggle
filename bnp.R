#' ---------------------------------------------------------------
#' @version 1.0.0
#' @title BNP Kaggle Data
#' 
#' @description 
#' This script is used to analyze the BNP data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#' Loading libraries
library(zoo)
library(Hmisc)
library(caret)
library(usdm)
library(h2o)
library(ROCR)

#' Set working directory
setwd("D:\\Kaggle\\BNP Paribas Cardif Claims Management\\")

#' Read data 
fullData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
dim(fullData)

# ' Temporaray data
train<- fullData[,-1]
table(is.na(train))

#' Remove columns has high NA
#train <- train[,colSums(is.na(train)) < 1500]
#dim(train)
#dropped 52 columns

table(is.na(train))
#' Convert text columns to numeric and impute by mode
for (f in 1:length(colnames(train))) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]], levels=levels)
    train[[f]] <- impute(train[[f]])
    train[[f]] <- as.numeric(train[[f]])
  }
}
for (f in 1:length(colnames(test))) {
  if (class(test[[f]])=="character") {
    levels <- unique(c(test[[f]]))
    test[[f]] <- factor(test[[f]], levels=levels)
    test[[f]] <- impute(test[[f]])
    test[[f]] <- as.numeric(test[[f]])
  }
}
dim(train)
table(is.na(train))

#' Impute continuous variables by mean
train <- apply(train, 2, impute)
test <- apply(test, 2, impute)
train <- as.data.frame(train)
test <- as.data.frame(test)

#' Preparing train and test
train$target<- factor(train$target)

#' Initialize and start H2o server
h2o.server <- h2o.init( nthreads= -1)

#' Convert data H2o data format for model development
train.hex <- as.h2o(train)
test.hex <- as.h2o(test[,-1])

#' Feature selection
features <- colnames(train[,c(-1)])

#' GBM Model1
gbmF_model_1 <- h2o.gbm( x=features,
                         y = "target",
                         training_frame =train.hex ,
                         max_depth = 4,
                         distribution = "bernoulli",
                         ntrees =500,
                         learn_rate = 0.04,
                         nbins_cats = 6000,
                         nfolds = 10
)
gbmF_model_1
varImp <- gbmF_model_1@model$variable_importances
View(varImp)
length(varImp$variable[varImp$relative_importance > 0])

testgbm_1 <- as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
output <- ""
output$id <- test[, 1]
output$predict <- testgbm_1$p1
write.csv(output, "FinalSubmission4.csv")

#' GBM Model2
gbmF_model_2 <- h2o.gbm( x=features,
                         y = "target",
                         training_frame =train.hex ,
                         max_depth = 3,
                         distribution = "bernoulli",
                         ntrees =600,
                         learn_rate = 0.04,
                         nbins_cats = 6400
)
gbmF_model_2

varImp2 <- gbmF_model_2@model$variable_importances
View(varImp2)
length(varImp2$variable[varImp2$relative_importance > 0])
testgbm_2 <- as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )

output <- ""
output$id <- test[, 1]
output$predict <- testdl_model_1
write.csv(output, "FinalSubmission3.csv")


#' GLM Model 1
glm_model_1 <- h2o.glm(x = features,
                       y = "NRR90",
                       training_frame = train.hex,
                       solver = "L_BFGS",
                       family = "binomial",
                       link = "logit",
                       alpha = 0,
                       lambda_search = TRUE,
                       max_iterations = 500
)
glm_model_1
h2o.varimp(glm_model_1)
View(h2o.varimp(glm_model_1))
length(varImp$variable[varImp$relative_importance > 0])
testglm_1 <- as.data.frame(h2o.predict(glm_model_1, newdata = test.hex) )
table(target, testglm_1$predict)
table(target)

#' Deep Learning Model 1
dl_model_1 <- h2o.deeplearning( x=features,
                                y = "target",
                                training_frame =train.hex ,
                                activation="Rectifier",
                                hidden=60,
                                epochs=60,
                                adaptive_rate =FALSE
)
dl_model_1
summary(dl_model_1)

testdl_model_1 <- as.data.frame(h2o.predict(dl_model_1, newdata = test.hex) )


#' Deep Learning Model 2
dl_model_2 <- h2o.deeplearning( x=features,
                                y = "NRR90",
                                training_frame =train.hex ,
                                activation = "Rectifier",
                                hidden=60,
                                epochs=40,
                                adaptive_rate =F
)
dl_model_2
summary(dl_model_2)
varImp4 <- dl_model_1@model$variable_importances
testdl_model_2 <- as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
table(target, testdl_model_2$predict)

#' Deep Learning Model 3
dl_model_3 <- h2o.deeplearning( x=features,
                                y = "NRR90",
                                training_frame =train.hex 
                                #validation_frame =testHex ,
)

testdl_model_3 <- as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )
table(target, testdl_model_3$predict)

#' AUC curve
a <- prediction(testdl_model_3$p1, target)
perf <- performance(a, measure = "tpr", x.measure = "fpr")
auc <- performance(a, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#' Finding potential customer segments
custClusterData <- clusterData[, c("gtntu14_risk_sc", "GTNEQ14_SC", "FZ4.LengthOfResidence.06", 
                                   "mailDateMonth", "FZ4.GenerationsInHousehold.1",
                                   "GTNEQ14_RG", "FZ4.HomeMarketValue.N", "gtntu14_sc",
                                   "GTNEQ14_TM", "Factor10", "G102", "FZ4.EstimatedHouseholdIncome.K",
                                   "FZ4.HomeMarketValue.F", "CENSUS10149", "HIGHCRD_CONSUMERFINANCE",
                                   "NumberOfChildren")]
custClusterData <- as.data.frame(custClusterData)

#' K means
kmeans <- kmeans(custClusterData, 4)
plot(kmeans, col = kmeans$cluster)
library(useful)
plot.kmeans(kmeans, data = custClusterData)

