# Run_analysis

if(!file.exists("UCIdata")){
  unzip(UCIdata)
}

##assigning data

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

##Merge train and test

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

##Extract mean and std deviation

CleanData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

##name activities 

CleanData$code <- activities[TidyData$code, 2]

##variable names


names(CleanData)[2] = "activity"
names(CleanData)<-gsub("Acc", "Accelerometer", names(CleanData))
names(CleanData)<-gsub("Gyro", "Gyroscope", names(CleanData))
names(CleanData)<-gsub("BodyBody", "Body", names(CleanData))
names(CleanData)<-gsub("Mag", "Magnitude", names(CleanData))
names(CleanData)<-gsub("^t", "Time", names(CleanData))
names(CleanData)<-gsub("^f", "Frequency", names(CleanData))
names(CleanData)<-gsub("tBody", "TimeBody", names(CleanData))
names(CleanData)<-gsub("-mean()", "Mean", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("-std()", "STD", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("-freq()", "Frequency", names(CleanData), ignore.case = TRUE)
names(CleanData)<-gsub("angle", "Angle", names(CleanData))
names(CleanData)<-gsub("gravity", "Gravity", names(CleanData))

##average of each variable 

FinalData <- CleanData %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)


