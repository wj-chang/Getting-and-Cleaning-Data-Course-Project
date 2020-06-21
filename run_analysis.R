library(dplyr)

# Download the dataset
if(!file.exists("./getcleandata")){dir.create("./getcleandata")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./getcleandata/projectdataset.zip")
unzip(zipfile = "./getcleandata/projectdataset.zip", exdir = "./getcleandata")

#1) Merges the training and the test sets to create one data set.
# 1.1 Read files

# 1.1.1 Read training datasets
x_train <- read.table("./getcleandata/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./getcleandata/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./getcleandata/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Read test datasets
x_test <- read.table("./getcleandata/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./getcleandata/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./getcleandata/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Read features
features <- read.table("./getcleandata/UCI HAR Dataset/features.txt")

# 1.1.4 Read activity labels
activitylabels = read.table("./getcleandata/UCI HAR Dataset/activity_labels.txt")

# 1.2 Assign variable names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activitylabels) <- c("activityID", "activityType")

# 1.3 Merging all datasets into one set
training <- cbind(y_train, subject_train, x_train)
test <- cbind(y_test, subject_test, x_test)
dataset <- rbind(training, test)

#2) Extracts only the measurements on the mean and standard deviation for each measurement
# 2.1 Read column names
colnames <- colnames(dataset)

# 2.2 Create vector to define ID, mean, and sd
mean_sd <- (grepl("activityID", colnames) |
            grepl("subjectID", colnames) |
            grepl("mean..", colnames) |
            grepl("std...", colnames))

# 2.3 Create data subset
tidydata <- dataset %>% select(subjectID, activityID, contains("mean"), contains("std"))

#3) Uses descriptive activity names to name the activities in the data set
tidydata$activityID <- activitylabels[tidydata$activityID, 2]

#4) Appropriately labels the data set with descriptive variable names.
# 4.1 Tidy column names
columns <- colnames(tidydata)
for (i in 1:length(columns)) {
        columns[i] <- gsub("\\()","",columns[i])
        columns[i] <- gsub("-std$","StdDev",columns[i])
        columns[i] <- gsub("-mean","Mean",columns[i])
        columns[i] <- gsub("^(t)","Time",columns[i])
        columns[i] <- gsub("^(f)","Frequency",columns[i])
        columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
        columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
        columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
        columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
        columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
        columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
        columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
}

#4.2 Label new descriptive column names
colnames(tidydata) <- columns

#4.3 Remove activityType column
tidydata <- tidydata[,names(tidydata) != 'activityType']

#5) Create a second, independent tidy data set with the average of each variable for each activity and each subject.
#5.1 Averaging each activity and each subject as Tidy Data
finaldata <- tidydata %>%  group_by(subjectID, activityID) %>% summarise_all(funs(mean))

#5.2 Export tidy data set
write.table(finaldata, "FinalData.txt", row.name=FALSE)


