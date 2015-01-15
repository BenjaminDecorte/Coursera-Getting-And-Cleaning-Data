#------------------------------------------------------------------------------
# Step1. Merges the training and the test sets to create one data set.
#------------------------------------------------------------------------------
setwd("C:/Users/Ben/Downloads/Gettin and Cleaning Data/")
trainingSet <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
trainingSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testSet <- read.table("UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
Data <- rbind(trainingSet, testSet)
Label <- rbind(trainingLabel, testLabel)
Subject <- rbind(trainingSubject, testSubject)

#-----------------------------------------------------------------------------
# Step2. Extracts only the measurements on the mean and standard
#-----------------------------------------------------------------------------
# deviation for each measurement.
features <- read.table("UCI HAR Dataset/features.txt")
meanStandardIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
Data <- Data[, meanStandardIndices]
names(Data) <- gsub("\\(\\)", "", features[meanStandardIndices, 2]) # delete ()
names(Data) <- gsub("mean", "Mean", names(Data)) # capitalize M
names(Data) <- gsub("std", "Std", names(Data)) # capitalize S
names(Data) <- gsub("-", "", names(Data)) # remove - 

#------------------------------------------------------------------------------------
# Step3. Uses descriptive activity names to name the activities in the data set
#------------------------------------------------------------------------------------
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[Label[, 1], 2]
Label[, 1] <- activityLabel
names(Label) <- "activity"

#-----------------------------------------------------------------------------------
# Step4. Appropriately labels the data set with descriptive activity names.
#-----------------------------------------------------------------------------------
names(Subject) <- "subject"
GOODDataset <- cbind(Subject, Label, Data)
write.table(GOODDataset, "GOODdataset.txt") # write out the 1st dataset

#-----------------------------------------------------------------------------------
# Step5. Creates a second, independent tidy data set with the average of
# each variable for each activity and each subject.
#-----------------------------------------------------------------------------------
subjectLength <- length(table(Subject)) # 30
activityLength <- dim(activity)[1] # 6
columnLength <- dim(GOODDataset)[2]
result <- matrix(NA, nrow=subjectLength*activityLength, ncol=columnLength)
result <- as.data.frame(result)
colnames(result) <- colnames(GOODDataset)
row <- 1
for(i in 1:subjectLength) {
  for(j in 1:activityLength) {
    result[row, 1] <- sort(unique(Subject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == GOODDataset$subject
    bool2 <- activity[j, 2] == GOODDataset$activity
    result[row, 3:columnLength] <- colMeans(GOODDataset[bool1&bool2, 3:columnLength])
    row <- row + 1
  }
}
write.table(result, "GOODDatasetWithMean.txt", row.name=FALSE)


