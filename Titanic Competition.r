## Download data from Kaggle ##

train.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/train.csv"
test.URL <- "http://www.kaggle.com/c/titanic-gettingStarted/download/test.csv"

train.data <- read.csv(train.URL, header = TRUE)
test.data <- read.csv(test.URL, header = TRUE)

