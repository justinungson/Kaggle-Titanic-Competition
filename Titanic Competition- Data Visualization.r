## Download data from Kaggle ##

## Create a Data Directory ##
if(!file.exists("Kaggle Titanic Data")){
  dir.create("Kaggle Titanic Data")
}

## I saved copies of the raw datasets on Github ##
train.URL <- "https://raw.githubusercontent.com/justinungson/Kaggle-Titanic-Competition/master/Kaggle%20Titanic%20Data/train.csv"
download.file(train.URL, destfile = "./Kaggle Titanic Data/train.csv")
train.date.downloaded <- date()

test.URL <- "https://raw.githubusercontent.com/justinungson/Kaggle-Titanic-Competition/master/Kaggle%20Titanic%20Data/test.csv"
download.file(test.URL, destfile = "./Kaggle Titanic Data/test.csv")
test.date.downloaded <- date()

list.files("./Kaggle Titanic Data")

train.data <- read.csv("./Kaggle Titanic Data/train.csv", header = TRUE, 
                       stringsAsFactors = FALSE)

test.data <- read.csv("./Kaggle Titanic Data/test.csv", header = TRUE, 
                      stringsAsFactors = FALSE)

## Load the Amelia Package for visualizing missing data ##
library(Amelia)
missmap(train.data, main = "Missing Data from the Titanic Training Dataset",
        col = c("orange", "black"), legend = FALSE)

## Exploratory Data Analysis ##
library(ggplot2)

train.data$Survived <- factor(train.data$Survived, levels = c(0,1), 
                              labels = c("Died", "Survived"))

train.data$Sex <- factor(train.data$Sex, levels = c("male", "female"), 
                         labels = c("male", "female"))

qplot(Survived, data = train.data, binwidth = 0.5)
qplot(Sex, data = train.data, binwidth = 0.5)
qplot(Pclass, data = train.data, binwidth = 0.5)
qplot(Age, data = train.data, binwidth = 2)
qplot(SibSp, data = train.data, binwidth = 0.5)
qplot(Parch, data = train.data, binwidth = 0.5)

## Dig a little deeper ##
qplot(Survived, data = train.data) + facet_wrap(~ Sex)
qplot(Survived, data = train.data) + facet_wrap(~ PClass)

