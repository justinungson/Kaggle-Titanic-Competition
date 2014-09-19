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
                       stringsAsFactors = FALSE, na.strings = c("", "NA"))

test.data <- read.csv("./Kaggle Titanic Data/test.csv", header = TRUE, 
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))

## Load the Amelia Package for visualizing missing data ##
library(Amelia)
missmap(train.data, main = "Missing Data from the Titanic Training Dataset",
        col = c("orange", "black"), legend = FALSE)


## Exploratory Data Analysis ##
library(ggplot2)

## First we must convert the character variables into quantitative variables so that we can
## fit our model.
train.data$Survived <- factor(train.data$Survived, levels = c(0, 1), 
                              labels = c("Died", "Survived"))

train.data$Sex <- factor(train.data$Sex, levels = c(0, 1), 
                         labels = c("male", "female"))

train.data$Embarked <- factor(train.data$Embarked, levels = c("S", "C", "Q"), 
                              labels = c("Southampton", "Cherbourg", "Queenstown"))

## From the mapping of missing data, we can see that the "Cabin" variable is missing ~80% of
## its data and the "Age" variable is missing ~35% of its data. The missing data for "Cabin"
## is too much for any imputation methods but we may be able to impute the "Age" variable.

master_vector = grep("Master.",train.data$Name, fixed = TRUE)
miss_vector = grep("Miss.", train.data$Name, fixed = TRUE)
ms_vector = grep("Ms.", train.data$Name, fixed = TRUE)
mrs_vector = grep("Mrs.", train.data$Name, fixed = TRUE)
mr_vector = grep("Mr.", train.data$Name, fixed = TRUE)
sir_vector = grep("Sir.", train.data$Name, fixed = TRUE)
dr_vector = grep("Dr.", train.data$Name, fixed = TRUE)
rev_vector = grep("Rev.", train.data$Name, fixed = TRUE)
maj_vector = grep("Major", train.data$Name, fixed = TRUE)
cap_vector = grep("Capt.", train.data$Name, fixed = TRUE)
col_vector = grep("Col.", train.data$Name, fixed = TRUE)
mlle_vector = grep("Mlle.", train.data$Name, fixed = TRUE)
mme_vector = grep("Mme.", train.data$Name, fixed = TRUE)

## These functions standardize the names associated with each passenger for easier
## analysis.

for(i in master_vector) {
  train.data$Name[i] = "Master"
}
for(i in miss_vector) {
  train.data$Name[i] = "Miss"
}
for(i in ms_vector) {
  train.data$Name[i] = "Miss"
}
for(i in mrs_vector) {
  train.data$Name[i] = "Mrs"
}
for(i in mr_vector) {
  train.data$Name[i] = "Mr"
}
for(i in sir_vector) {
  train.data$Name[i] = "Sir"
}
for(i in dr_vector) {
  train.data$Name[i] = "Dr"
}
for(i in rev_vector) {
  train.data$Name[i] = "Rev"
}
for(i in maj_vector) {
  train.data$Name[i] = "Maj"
}
for(i in cap_vector) {
  train.data$Name[i] = "Cap"
}
for(i in col_vector) {
  train.data$Name[i] = "Col"
}
for(i in mlle_vector) {
  train.data$Name[i] = "Mlle"
}
for(i in mme_vector) {
  train.data$Name[i] = "Mme"
}

master_age = round(median(train.data$Age[train.data$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(median(train.data$Age[train.data$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(median(train.data$Age[train.data$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(median(train.data$Age[train.data$Name == "Mr"], na.rm = TRUE), digits = 2)
sir_age = round(median(train.data$Age[train.data$Name == "Sir"], na.rm = TRUE), digits = 2)
dr_age = round(median(train.data$Age[train.data$Name == "Dr"], na.rm = TRUE), digits = 2)
rev_age = round(median(train.data$Age[train.data$Name == "Rev"], na.rm = TRUE), digits = 2)
maj_age =round(median(train.data$Age[train.data$Name == "Maj"], na.rm = TRUE), digits = 2)
cap_age = round(median(train.data$Age[train.data$Name == "Cap"], na.rm = TRUE), digits = 2)
col_age = round(median(train.data$Age[train.data$Name == "Col"], na.rm = TRUE), digits = 2)
mlle_age = round(median(train.data$Age[train.data$Name == "Mlle"], na.rm = TRUE), digits = 2)
mme_age = round(median(train.data$Age[train.data$Name == "Mme"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(train.data)) {
  if (is.na(train.data[i,5])) {
    if (train.data$Name[i] == "Master") {
      train.data$Age[i] = master_age
    } else if (train.data$Name[i] == "Miss") {
      train.data$Age[i] = miss_age
    } else if (train.data$Name[i] == "Mrs") {
      train.data$Age[i] = mrs_age
    } else if (train.data$Name[i] == "Mr") {
      train.data$Age[i] = mr_age
    } else if (train.data$Name[i] == "Sir") {
      train.data$Age[i] = mr_age
    } else if (train.data$Name[i] == "Dr") {
      train.data$Age[i] = dr_age
    } else if (train.data$Name[i] == "Rev"){
      train.data$Age[i] = rev_age
    } else if (train.data$Name[i] == "Maj") {
      train.data$Age[i] = maj_age
    } else if (train.data$Name[i] == "Cap") {
      train.data$Age[i] = cap_age
    } else if (train.data$Name[i] == "Col") {
      train.data$Age[i] = col_age
    } else if (train.data$Name[i] == "Mlle") {
      train.data$Age[i] = mlle_age
    } else if (train.data$Name[i] == "Mme") {
      train.data$Age[i] = mme_age
    } else {
      print("Uncaught Title")
    }
  }
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

