#' ---------------------------------------------------------------
#' @version Kaggle San Francisco Crime Classification 
#' @title Classification
#' 
#' @description 
#' This script is used to analyze the san francisco crime data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------


#set working directory
setwd("D:\\Kaggle\\San Francisco Crime Classification\\")

#loading data
train <- read.csv("train.csv\\train.csv")
test <-  read.csv("test.csv\\test.csv")

str(train)
