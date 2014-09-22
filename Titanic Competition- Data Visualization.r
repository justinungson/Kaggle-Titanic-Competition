## Download data from Kaggle ##
library(Amelia)
library(ggplot2)
library(Hmisc)

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
                       stringsAsFactors = FALSE, na.strings = c("", "NA"))

test.data <- read.csv("./Kaggle Titanic Data/test.csv", header = TRUE, 
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))

## Load the Amelia Package for visualizing missing data ##
missmap(train.data, main = "Missing Data from the Titanic Training Dataset",
        col = c("orange", "black"), legend = FALSE)

## First we must convert the character variables into factor variables so that we can fit our model.
train.data$Survived <- factor(train.data$Survived)
levels(train.data$Survived) = c("Died", "Survived")

train.data$Sex <- factor(train.data$Sex)

train.data$Embarked <- factor(train.data$Embarked)
levels(train.data$Embarked) = c("Cherbourg", "Queenstown", "Southampton")

## From the mapping of missing data, we can see that the "Cabin" variable is missing too many records for any
## accurate imputation methods. The "Age" variable is also missing data (around 20% for the nearly 900 records).
## We could just take the mean (29.7) or median (28.0) age value but we may be albe to use a more refined imputation
## methodology by taking advantage of the titles of each individual.

get.title <- function(data) {
  title.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.end <- title.start + attr(title.start, "match.length")-1
  data$Title <- substr(data$Name, title.start+2, title.end-1)
  return (data$Title)
}  

train.data$Title <- get.title(train.data)

bystats(train.data$Age, train.data$Title, fun = function(x) c(Mean = mean(x), Median = median(x)))

train.data.missing.titles <- c("Dr", "Master", "Miss", "Mr", "Mrs")

impute.median.age <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[which(filter.var == v)] <- impute(impute.var[which(filter.var == v)])
  }
  return(impute.var)
}

train.data$Age <- impute.median.age(train.data$Age, train.data$Title, train.data.missing.titles)

## The "Embarked" variable contains 2 missing values. We can impute the data with the most common point of
## departure (Queenstown)

train.data$Embarked[which(is.na(train.data$Embarked))] <- "Southampton"

## Create child, mother, and family variables ##
train.data["Child"]
for(i in 1:nrow(train.data)) {
  if(train.data$Age[i] <= 12) {
    train.data$Child = 1
  } else {
    train.data$child = 0
  }
  }
}

train.data["Mother"]
for(i in 1:nrow(train.data)) {
  if(train.data$Name[i] == "Mrrs" & tran.data$Parch[i] > 0) {
    train.data$Mother = 1
  } else {
    train.data$Mother = 0
  }
}

train.data["Family"]
for(i in 1:nrow(train.data)) {
  x = train.data$SibSp[i]
  y = train.data$Parch[i]
  train.data$Family = x + y + 1
}

qplot(Survived, data = train.data, binwidth = 0.5)
qplot(Sex, data = train.data, binwidth = 0.5)
qplot(Pclass, data = train.data, binwidth = 0.5)
qplot(Age, data = train.data, binwidth = 2)
qplot(SibSp, data = train.data, binwidth = 0.5)
qplot(Parch, data = train.data, binwidth = 0.5)

## Dig a little deeper ##
qplot(Survived, data = train.data) + facet_wrap(~ Sex)
qplot(Survived, data = train.data) + facet_wrap(~ PClass)

