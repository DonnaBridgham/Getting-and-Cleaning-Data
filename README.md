Getting and Cleaning Data Course Project
Companies like FitBit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked are collected from the accelerometers from the Samsung Galaxy S smartphone.

A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data is available at:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The aim of the project is to clean and extract usable data from the above zip file. R script called run_analysis.R that does the following:
•	Merges the training and the test sets to create one data set.
•	Extracts only the measurements on the mean and standard deviation for each measurement.
•	Uses descriptive activity names to name the activities in the data set
•	Appropriately labels the data set with descriptive variable names.
•	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

In this repository, you find:
•	run_analysis.R : the R-code run on the data set
•	tidy.txt : the clean data extracted from the original data using run_analysis.R
•	CodeBook.md : the CodeBook reference to the variables in Tidy.txt
•	README.md : the analysis of the code in run_analysis.R

Desktop
1.	Download the data source and put into a folder on your local drive. You'll have a UCI HAR Dataset folder.
2.	Put run_analysis.R in the parent folder of UCI HAR Dataset, then set it as your working directory using setwd() function in RStudio.
3.	Run source("run_analysis.R"), then it will generate a new file data.txt in your working directory.

