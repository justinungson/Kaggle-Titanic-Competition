## Download data from Kaggle ##

if(!file.exists("Kaggle Titanic Data")){
  dir.create("Kaggle Titanic Data")
}

train.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/train.csv"
download.file(train.URL, destfile = "./data/train.csv")
train.date.downloaded <- date()

test.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/test.csv"
download.file(test.URL, destfile = "./data/test.csv")
test.date.downloaded <- date()

list.files("./data")

train.data <- read.csv("./data/train.csv", header = TRUE, 
                        stringsAsFactors = FALSE)
test.data <- read.csv("./data/test.csv", header = TRUE, 
                        stringsAsFactors = FALSE)
