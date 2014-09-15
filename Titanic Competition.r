setwd("Users/justinungson/dropbox/R/data")

##Load the data##
train.data <- read.csv("kaggle_train.csv", header = TRUE, stringsAsFactors = FALSE)
test.data <- read.csv("kaggle_test.csv", header = TRUE, stringsAsFactors = FALSE)

##Make Plots and Explore the data##
plot(density(train.data$Age, na.rm = TRUE))
plot(density(train.data$fare, na.rm = TRUE))
counts <- table(train.data$Survived, train.data$Sex)
barplot(counts,
         xlab = "Gender"
         ylab = "Number of People"
         main = "Survival by Gender")

class.survival <- table(train.data$Survived, train.data$Pclass)
barplot(class.survival
         xlab = "Passenger Cabin Class"
         ylab = "Number of People"
         main = "Survived and Deceased by Passenger Cabin Class")

train.data <- train.data[-c(1,9:12)]
train.data$Sex <- gsub("female", 1, train.data$Sex)
train.data$Sex <- gsub("male", 0, train.data$Sex)

## Fill in Missing Age Values##
master.vector <- grep("Master\\.", train.data$Name)
miss.vector <- grep("Miss\\.", train.data$Name)
mrs.vector <- grep("Mrs\\.", train.data$Name)
mr.vector <- grep("Mr\\.", train.data$Name)
dr.vector <- grep("Dr\\.", train.data$Name)

##Rename Each Person According to Age##
for(i in master.vector){
	train.data$Name[i] <- "Master"
}
for(i in miss.vector){
	train.data$Name[i] <- "Miss"
}
for(i in mrs.vector){
	train.data$Name[i] <- "Mrs"
}
for(i in mr.vector){
	train.data$Name[i] <- "Mr"
}
for(i in dr.vector){
	train.data$Name[i] <- "Dr"
}

##Replace Ages with Group Averages##
master.age <- round(mean(train.data$Age[train.data$Name == "Master"], na.rm = TRUE) digits = 2)
miss.age <- round(mean(train.data$Age[train.data$Name == "Miss"], na.rm = TRUE) digits = 2)
mrs.age <- round(mean(train.data$Age[train.data$Name == "Mrs"], na.rm = TRUE), digits - 2)
mr.age <- round(mean(train.data$Age[train.data$Name == "Mr"], na.rm = TRUE) digits = 2)
dr.age <- round(mean(train.data$Age[train.data$Name == "Dr"], na.rm = TRUE) digits = 2)

for(i in 1:nrow(train.data)) {
	if(is.na(train.data[i, 5])) {
		if(train.data$Name[i] == "Master") {
			train.data$Age[i] <- master.age

		}
	}
}
