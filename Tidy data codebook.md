CodeBook
---------------------------------------------------------------
This document describes the data and transofrmations used by *run_analysis.R* and the definition of variables in *Tidy.txt*.

##Dataset Used

The data are obtained from "Human Activity Recognition Using Smartphones Data Set". The data are collected from the accelerometers from the Samsung Galaxy S smartphone. 
A full description is available at the site <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.

The data set used can be downloaded from <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>. 

##Input Data Set

The input data containts the following data files:

	"X_train.txt" = variable features intended for training.
	"y_train.txt" = activities corresponding to "X_train.txt".
	"subject_train.txt" = information on subjects from whom data is collected.
	"X_test.txt" = variable features intended for testing.
	"y_test.txt" = the activities corresponding to "X_test.txt".
	"subject_test.txt" = information on subjects from whom data is collected.
	"activity_labels.txt" = metadata on the types of activities.
	"features.txt" = the names of the features in the data sets.

##Transformations that were performed on the input dataset:

	"X_train.txt" read into "featuresTrain".
	"y_train.txt" read into "activityTrain".
	"subject_train.txt" read into "subjectTrain".
	"X_test.txt" read into "featuresTest".
	"y_test.txt" read into "activityTest".
	"subject_test.txt" read into "subjectTest".
	"features.txt" read into "featureNames".
	"activity_labels.txt" read into "activityLabels".

	The subjects in training and test set data are merged to form "subject".
	The activities in training and test set data are merged to form "activity".
	The features of test and training are merged to form "features".
	The name of the features are set in "features" from "featureNames".
	"features", "activity" and "subject" are merged to form "completeData".
	Indices of columns that contain std or mean, activity and subject are taken into "requiredColumns".
	"extractData" is created with data from columns in "requiredColumns".
	"Activity" column in "extractData" is updated with descriptive names of activities taken from "activityLabels".
	"Activity" column is expressed as a factor variable.
	Acronyms in variable names in "extractData", like "Acc", "Gyro", "Mag", "t" and "f" are replaced with descriptive labels including "Accelerometer", "Gyroscope", "Magnitude", "Time" and "Frequency".
	"tidyData" is created as a set with average for each activity and subject of "extractData", entries are ordered based on activity and subject.
	"tidyData" is written into "Tidy.txt".

##Output Data Set

	The output data "Tidy.txt" is a a space-delimited value file. 
	The header line contains the names of the variables. 
	The data contains the mean and standard deviation values of the data contained in the input files. 
 
