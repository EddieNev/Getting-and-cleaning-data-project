##Load libraries

library(data.table)
library(dplyr)

##Read metadata

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

##Format the train and test data sets

##Read train data

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

##Read test data

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

##Task 1 - Merge the train and test data to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##Name the columns

colnames(features) <- t(featureNames[2])

##Merge the data

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

##Task 2 - Extract only the measurements on the mean and standard deviation for each measurement

##Extract the column indices that have either mean or std in them.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

##Add activity and subject columns to the list and look at the dimension of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

##Create extractData with the selected columns in requiredColumns and look at dimension of requiredColumns.

extractData <- completeData[,requiredColumns]
dim(extractData)

##Task 3 - Use descriptive activity names to name the activities in the data set

##activity field in extractData is numeric so convert to character 

extractData$Activity <- as.character(extractData$Activity)
for (i in 1:6){
extractData$Activity[extractData$Activity == i] <- as.character(activityLabels[i,2])
}

##Factor the activity variable now that activity names are updated.

extractData$Activity <- as.factor(extractData$Activity)

##Task 4 - Appropriately label the data set with descriptive variable names

names(extractData)<-gsub("Acc", "Accelerometer", names(extractData))
names(extractData)<-gsub("Gyro", "Gyroscope", names(extractData))
names(extractData)<-gsub("BodyBody", "Body", names(extractData))
names(extractData)<-gsub("Mag", "Magnitude", names(extractData))
names(extractData)<-gsub("^t", "Time", names(extractData))
names(extractData)<-gsub("^f", "Frequency", names(extractData))
names(extractData)<-gsub("tBody", "TimeBody", names(extractData))
names(extractData)<-gsub("-mean()", "Mean", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-std()", "STD", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-freq()", "Frequency", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("angle", "Angle", names(extractData))
names(extractData)<-gsub("gravity", "Gravity", names(extractData))


##Task 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

##Set Subject as a factor variable.

extractData$Subject <- as.factor(extractData$Subject)
extractData <- data.table(extractData)

##Create tidyData as a data set, order tidyData then write it to data file Tidy2.txt
 
tidyData <- aggregate(. ~Subject + Activity, extractData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)


