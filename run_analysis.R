##Load packages data.table, and dplyr

## Check directory

##Load variables into featureNames and activityLabels

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE

##Format Test and Training Data
## The training and test data sets are categorized into subject, activity and features

## Start by reading the Train Data

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## Then bring in the Test Data

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

##Merge the Data to create one Complete Data Set

#Store the data in subject, activity and features


subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##Name the columns by Feature

colnames(features) <- t(featureNames[2])

##Merge the Data in completeData to represent the complete data set

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

## Take only the mean and standard deviation for each measurement

##extract the indexes

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

##look at the dimension of completeData


requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)



##extract data once again

extractedData <- completeData[,requiredColumns]
dim(extractedData)




##Change the data type from numeric to string


extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

##Now account for activity

extractedData$Activity <- as.factor(extractedData$Activity)

##Name the variables from the extracted variables
names(extractedData)

#Acc can be replaced with Accelerometer
#Gyro can be replaced with Gyroscope
#BodyBody can be replaced with Body
#Mag can be replaced with Magnitude
#Character f can be replaced with Frequency
#Character t can be replaced with Time

 


names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)
   

##Subject Will Now be the variable 

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

##Average Activity and Subject and order the data in tidy.txt
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)