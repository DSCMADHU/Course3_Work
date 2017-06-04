#1. Download Files
setwd("D://R_Learning//Data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Dataset.zip")
unzip(zipfile="./Dataset.zip",exdir="./Source_data")

#2. Set Working Directory

setwd("D://R_Learning//Data//Source_data//UCI HAR Dataset")

#3. Merge the training and the test data.

#reading the data general and training.

features        <- read.table("./features.txt",header=FALSE)
activityLabel   <- read.table("./activity_labels.txt",header=FALSE)
subjectTrain    <-read.table("./train/subject_train.txt", header=FALSE)
xTrain          <- read.table("./train/X_train.txt", header=FALSE)
yTrain          <- read.table("./train/y_train.txt", header=FALSE)


#Assign column names to the data above.

colnames(activityLabel)<-c("activityId","activityType")
colnames(subjectTrain) <- "subId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"


#Merging training Data...

trainData <- cbind(yTrain,subjectTrain,xTrain)

#Reading the test Data

subjectTest    <-read.table("./test/subject_test.txt", header=FALSE)
xTest         <- read.table("./test/X_test.txt", header=FALSE)
yTest         <- read.table("./test/y_test.txt", header=FALSE)

# Assign column names.. same as for training data..

colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

# merging test Data
testData <- cbind(yTest,subjectTest,xTest)


#final merged data

finalData <- rbind(trainData,testData)

# creating a vector for column names to be used further

colNames <- colnames(finalData);



# 4. Extract only the measurements on the mean and standard deviation for each measurement


subFeaturesNames<-features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)]
selectedNames<-c("activityId","subId",as.character(subFeaturesNames) )
data_mean_std<-subset(finalData,select=selectedNames)

colnames(data_mean_std)

#5. #Uses descriptive activity names to name the activities in the data set


install.packages("plyr")
library(plyr)


data_mean_std <- join(data_mean_std,activityLabel, by = "activityId", match = "first")

#Remove column ActivityId

data_mean_std <-data_mean_std[,-1]

#6. Appropriately labels the data set with descriptive variable names.

#Remove parentheses

names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)

#correct syntax in names

names(data_mean_std) <- make.names(names(data_mean_std))

#add descriptive names

names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#7. creates a second, independent tidy data set with the average of each variable for each activity and each subject.


aggr.data <- aggregate(data_mean_std[, 2:ncol(data_mean_std)-1],
                       by=list(activity = data_mean_std$activityType,
                               
                               ),
                       mean)

selectedNames_new<-c("activity","Subject",colnames(aggr.data[,grep("Mean|Std",colnames(aggr.data))]) )
aggr.data<-subset(aggr.data,select=selectedNames_new)

write.table(format(aggr.data, scientific=T), "mytidy.txt",
            row.names=F, col.names=T, quote=2)




