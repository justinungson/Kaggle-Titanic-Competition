## Download data from Kaggle ##

## Create a Data Directory ##
if(!file.exists("Kaggle Titanic Data")){
  dir.create("Kaggle Titanic Data")
}

train.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/train.csv"
download.file(train.URL, destfile = "./Kaggle Titanic Data/train.csv")
train.date.downloaded <- date()

test.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/test.csv"
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
        col = c("orange", "blue"), legend = FALSE)

## Exploratory Data Analysis ##
library(ggplot2)

qplot(survived, data = train.data, binwidth = 1)
qplot(sex, data = train.data, binwidth = 1)
qplot(pclass, data = train.data, binwidth = 1)
qplot(age, data = train.data, binwidth = 1)
qplot(sibsp, data = train.data, binwidth = 1)
qplot(parch, data = train.data, binwidth = 1)

