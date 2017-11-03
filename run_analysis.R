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

[1] 10299   563

##extract data once again

extractedData <- completeData[,requiredColumns]
dim(extractedData)

[1] 10299    88


##Change the data type from numeric to string

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

##Now account for activity

extractedData$Activity <- as.factor(extractedData$Activity)

##Name the variables from the extracted variables
names(extractedData)

 [1] "tBodyAcc-mean()-X"                    "tBodyAcc-mean()-Y"                   
 [3] "tBodyAcc-mean()-Z"                    "tBodyAcc-std()-X"                    
 [5] "tBodyAcc-std()-Y"                     "tBodyAcc-std()-Z"                    
 [7] "tGravityAcc-mean()-X"                 "tGravityAcc-mean()-Y"                
 [9] "tGravityAcc-mean()-Z"                 "tGravityAcc-std()-X"                 
[11] "tGravityAcc-std()-Y"                  "tGravityAcc-std()-Z"                 
[13] "tBodyAccJerk-mean()-X"                "tBodyAccJerk-mean()-Y"               
[15] "tBodyAccJerk-mean()-Z"                "tBodyAccJerk-std()-X"                
[17] "tBodyAccJerk-std()-Y"                 "tBodyAccJerk-std()-Z"                
[19] "tBodyGyro-mean()-X"                   "tBodyGyro-mean()-Y"                  
[21] "tBodyGyro-mean()-Z"                   "tBodyGyro-std()-X"                   
[23] "tBodyGyro-std()-Y"                    "tBodyGyro-std()-Z"                   
[25] "tBodyGyroJerk-mean()-X"               "tBodyGyroJerk-mean()-Y"              
[27] "tBodyGyroJerk-mean()-Z"               "tBodyGyroJerk-std()-X"               
[29] "tBodyGyroJerk-std()-Y"                "tBodyGyroJerk-std()-Z"               
[31] "tBodyAccMag-mean()"                   "tBodyAccMag-std()"                   
[33] "tGravityAccMag-mean()"                "tGravityAccMag-std()"                
[35] "tBodyAccJerkMag-mean()"               "tBodyAccJerkMag-std()"               
[37] "tBodyGyroMag-mean()"                  "tBodyGyroMag-std()"                  
[39] "tBodyGyroJerkMag-mean()"              "tBodyGyroJerkMag-std()"              
[41] "fBodyAcc-mean()-X"                    "fBodyAcc-mean()-Y"                   
[43] "fBodyAcc-mean()-Z"                    "fBodyAcc-std()-X"                    
[45] "fBodyAcc-std()-Y"                     "fBodyAcc-std()-Z"                    
[47] "fBodyAcc-meanFreq()-X"                "fBodyAcc-meanFreq()-Y"               
[49] "fBodyAcc-meanFreq()-Z"                "fBodyAccJerk-mean()-X"               
[51] "fBodyAccJerk-mean()-Y"                "fBodyAccJerk-mean()-Z"               
[53] "fBodyAccJerk-std()-X"                 "fBodyAccJerk-std()-Y"                
[55] "fBodyAccJerk-std()-Z"                 "fBodyAccJerk-meanFreq()-X"           
[57] "fBodyAccJerk-meanFreq()-Y"            "fBodyAccJerk-meanFreq()-Z"           
[59] "fBodyGyro-mean()-X"                   "fBodyGyro-mean()-Y"                  
[61] "fBodyGyro-mean()-Z"                   "fBodyGyro-std()-X"                   
[63] "fBodyGyro-std()-Y"                    "fBodyGyro-std()-Z"                   
[65] "fBodyGyro-meanFreq()-X"               "fBodyGyro-meanFreq()-Y"              
[67] "fBodyGyro-meanFreq()-Z"               "fBodyAccMag-mean()"                  
[69] "fBodyAccMag-std()"                    "fBodyAccMag-meanFreq()"              
[71] "fBodyBodyAccJerkMag-mean()"           "fBodyBodyAccJerkMag-std()"           
[73] "fBodyBodyAccJerkMag-meanFreq()"       "fBodyBodyGyroMag-mean()"             
[75] "fBodyBodyGyroMag-std()"               "fBodyBodyGyroMag-meanFreq()"         
[77] "fBodyBodyGyroJerkMag-mean()"          "fBodyBodyGyroJerkMag-std()"          
[79] "fBodyBodyGyroJerkMag-meanFreq()"      "angle(tBodyAccMean,gravity)"         
[81] "angle(tBodyAccJerkMean),gravityMean)" "angle(tBodyGyroMean,gravityMean)"    
[83] "angle(tBodyGyroJerkMean,gravityMean)" "angle(X,gravityMean)"                
[85] "angle(Y,gravityMean)"                 "angle(Z,gravityMean)"                
[87] "Activity"                             "Subject"   

> names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
> names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
> names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
> names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
> names(extractedData)<-gsub("^t", "Time", names(extractedData))
> names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
> names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
> names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
> names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
> names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
> names(extractedData)<-gsub("angle", "Angle", names(extractedData))
> names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
> names(extractedData)
 [1] "TimeBodyAccelerometerMean()-X"                     "TimeBodyAccelerometerMean()-Y"                    
 [3] "TimeBodyAccelerometerMean()-Z"                     "TimeBodyAccelerometerSTD()-X"                     
 [5] "TimeBodyAccelerometerSTD()-Y"                      "TimeBodyAccelerometerSTD()-Z"                     
 [7] "TimeGravityAccelerometerMean()-X"                  "TimeGravityAccelerometerMean()-Y"                 
 [9] "TimeGravityAccelerometerMean()-Z"                  "TimeGravityAccelerometerSTD()-X"                  
[11] "TimeGravityAccelerometerSTD()-Y"                   "TimeGravityAccelerometerSTD()-Z"                  
[13] "TimeBodyAccelerometerJerkMean()-X"                 "TimeBodyAccelerometerJerkMean()-Y"                
[15] "TimeBodyAccelerometerJerkMean()-Z"                 "TimeBodyAccelerometerJerkSTD()-X"                 
[17] "TimeBodyAccelerometerJerkSTD()-Y"                  "TimeBodyAccelerometerJerkSTD()-Z"                 
[19] "TimeBodyGyroscopeMean()-X"                         "TimeBodyGyroscopeMean()-Y"                        
[21] "TimeBodyGyroscopeMean()-Z"                         "TimeBodyGyroscopeSTD()-X"                         
[23] "TimeBodyGyroscopeSTD()-Y"                          "TimeBodyGyroscopeSTD()-Z"                         
[25] "TimeBodyGyroscopeJerkMean()-X"                     "TimeBodyGyroscopeJerkMean()-Y"                    
[27] "TimeBodyGyroscopeJerkMean()-Z"                     "TimeBodyGyroscopeJerkSTD()-X"                     
[29] "TimeBodyGyroscopeJerkSTD()-Y"                      "TimeBodyGyroscopeJerkSTD()-Z"                     
[31] "TimeBodyAccelerometerMagnitudeMean()"              "TimeBodyAccelerometerMagnitudeSTD()"              
[33] "TimeGravityAccelerometerMagnitudeMean()"           "TimeGravityAccelerometerMagnitudeSTD()"           
[35] "TimeBodyAccelerometerJerkMagnitudeMean()"          "TimeBodyAccelerometerJerkMagnitudeSTD()"          
[37] "TimeBodyGyroscopeMagnitudeMean()"                  "TimeBodyGyroscopeMagnitudeSTD()"                  
[39] "TimeBodyGyroscopeJerkMagnitudeMean()"              "TimeBodyGyroscopeJerkMagnitudeSTD()"              
[41] "FrequencyBodyAccelerometerMean()-X"                "FrequencyBodyAccelerometerMean()-Y"               
[43] "FrequencyBodyAccelerometerMean()-Z"                "FrequencyBodyAccelerometerSTD()-X"                
[45] "FrequencyBodyAccelerometerSTD()-Y"                 "FrequencyBodyAccelerometerSTD()-Z"                
[47] "FrequencyBodyAccelerometerMeanFreq()-X"            "FrequencyBodyAccelerometerMeanFreq()-Y"           
[49] "FrequencyBodyAccelerometerMeanFreq()-Z"            "FrequencyBodyAccelerometerJerkMean()-X"           
[51] "FrequencyBodyAccelerometerJerkMean()-Y"            "FrequencyBodyAccelerometerJerkMean()-Z"           
[53] "FrequencyBodyAccelerometerJerkSTD()-X"             "FrequencyBodyAccelerometerJerkSTD()-Y"            
[55] "FrequencyBodyAccelerometerJerkSTD()-Z"             "FrequencyBodyAccelerometerJerkMeanFreq()-X"       
[57] "FrequencyBodyAccelerometerJerkMeanFreq()-Y"        "FrequencyBodyAccelerometerJerkMeanFreq()-Z"       
[59] "FrequencyBodyGyroscopeMean()-X"                    "FrequencyBodyGyroscopeMean()-Y"                   
[61] "FrequencyBodyGyroscopeMean()-Z"                    "FrequencyBodyGyroscopeSTD()-X"                    
[63] "FrequencyBodyGyroscopeSTD()-Y"                     "FrequencyBodyGyroscopeSTD()-Z"                    
[65] "FrequencyBodyGyroscopeMeanFreq()-X"                "FrequencyBodyGyroscopeMeanFreq()-Y"               
[67] "FrequencyBodyGyroscopeMeanFreq()-Z"                "FrequencyBodyAccelerometerMagnitudeMean()"        
[69] "FrequencyBodyAccelerometerMagnitudeSTD()"          "FrequencyBodyAccelerometerMagnitudeMeanFreq()"    
[71] "FrequencyBodyAccelerometerJerkMagnitudeMean()"     "FrequencyBodyAccelerometerJerkMagnitudeSTD()"     
[73] "FrequencyBodyAccelerometerJerkMagnitudeMeanFreq()" "FrequencyBodyGyroscopeMagnitudeMean()"            
[75] "FrequencyBodyGyroscopeMagnitudeSTD()"              "FrequencyBodyGyroscopeMagnitudeMeanFreq()"        
[77] "FrequencyBodyGyroscopeJerkMagnitudeMean()"         "FrequencyBodyGyroscopeJerkMagnitudeSTD()"         
[79] "FrequencyBodyGyroscopeJerkMagnitudeMeanFreq()"     "Angle(TimeBodyAccelerometerMean,Gravity)"         
[81] "Angle(TimeBodyAccelerometerJerkMean),GravityMean)" "Angle(TimeBodyGyroscopeMean,GravityMean)"         
[83] "Angle(TimeBodyGyroscopeJerkMean,GravityMean)"      "Angle(X,GravityMean)"                             
[85] "Angle(Y,GravityMean)"                              "Angle(Z,GravityMean)"                             
[87] "Activity"                                          "Subject"   

##Subject Will Now be the variable 

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

##Average Activity and Subject and order the data in tidy.txt
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)