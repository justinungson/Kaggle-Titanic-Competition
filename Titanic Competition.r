## Download data from Kaggle ##

if(!file.exists("data")){
  dir.create("data")
}

train.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/train.csv"
download.file(train.URL, destfile = "./data/train.csv")
train.date.downloaded <- date()

test.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/test.csv"
download.file(test.URL, destfile = "./data/test.csv")
test.date.downloaded <- date()

train.data <- read.csv("./data/train.csv", header = TRUE, 
                        stringsAsFactors = FALSE)
test.data <- read.csv("./data/test.csv", header = TRUE, 
                        stringsAsFactors = FALSE)
