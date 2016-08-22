library(plyr)
# Merges the training and the test sets to create one data set.
setwd('/Users/nagasreechandra/DatascienceCourseNC-Reponew/GettingDataCourse')

# Read data from text files features and activityLabels
features <- read.table('./UCIHARDataset/features.txt',header=FALSE)
activityLabels <- read.table('./UCIHARDataset/activity_labels.txt',header=FALSE)

# Read Subject, x and Y from train and test
subjectTrain <- read.table('./UCIHARDataset/train/subject_train.txt',header=FALSE)
subjectTest <- read.table('./UCIHARDataset/test/subject_test.txt',header=FALSE)

# X
xTrain <- read.table('./UCIHARDataset/train/x_train.txt',header=FALSE)
xTest <- read.table('./UCIHARDataset/test/x_test.txt',header=FALSE)

# Y
yTrain <- read.table('./UCIHARDataset/train/y_train.txt',header=FALSE)
yTest <- read.table('./UCIHARDataset/test/y_test.txt',header=FALSE)

# Assigin column names
colnames(activityType) <- c('activityId','activityType')
colnames(subjectTrain)  <- "subjectId"
colnames(subjectTest) <- "subjectId"

colnames(xTrain) <- features[,2] 
colnames(xTest)  <- features[,2]

colnames(yTrain) <- "activityId"
colnames(yTest) <- "activityId"

subjectCompleteData <- rbind(subjectTrain, subjectTest)
xCompleteData<- rbind(xTrain, xTest)
yCompleteData<- rbind(yTrain, yTest) #Activity

# Merge Columns
combinedYandSData <- cbind(subjectCompleteData, yCompleteData)
mergedData <- cbind(xCompleteData, combinedYandSData)

# Extracts only the measurements on the mean and standard deviation for each measurement.

meanandstdFeatureNames <-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
featureNames <- c(as.character(meanandstdFeatureNames),"subject", "activity")
meanandstdData <- subset(mergedData, featureNames == TRUE)

# Uses descriptive activity names to name the activities in the data set

mergedData$activities <- as.character(mergedData$activities)
mergedData$activities[mergedData$activities == 1] <- "Walking"
mergedData$activities[mergedData$activities == 2] <- "Walking UPSTAIRS"
mergedData$activities[mergedData$activities == 3] <- "Walking Downstairs"
mergedData$activities[mergedData$activities == 4] <- "Sitting"
mergedData$activities[mergedData$activities == 5] <- "Standing"
mergedData$activities[mergedData$activities == 6] <- "Laying"
mergedData$activities <- as.factor(mergedData$activities)

#Appropriately labels the data set with descriptive variable names.

names(mergedData) <- gsub("Acc", "Accelerator",names(mergedData))
names(mergedData) <- gsub("Gyro", "Gyroscope",names(mergedData))
names(mergedData) <- gsub("Mag", "Magnitude",names(mergedData))
names(mergedData) <- gsub("^f", "frequency",names(mergedData))
names(mergedData) <- gsub("^t", "time",names(mergedData))

#From the data set in step 4, creates a second,
#independent tidy data set with the average of each variable for each activity and each subject.

mergedData.dt <- data.table(mergedData)
#This takes the mean of every column broken down by participants and activities
TidyData <- mergedData.dt[, lapply(.SD, mean), by = 'subjectId,activityId']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
