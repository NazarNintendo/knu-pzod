setwd("C:/Users/User/Desktop/FIT/PZOD/Lab4")

# Data preparation.

features <- read.table("UCI HAR Dataset/features.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
x_train$subject = subject_train$V1
x_train$activity = y_train$V1
x_test$subject = subject_test$V1
x_test$activity = y_test$V1

# 1. Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
colnames(X) <- c(features$V2, "subject", "activity")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
X_mean_std <- X[, features$V2 %in% features[grepl("mean", features$V2) | grepl("std", features$V2), ]$V2]

# 3. Uses descriptive activity names to name the activities in the data set.
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
X$activity <- sapply(X$activity, function(x) activities[x,]$V2)
  
# 4. Appropriately labels the data set with descriptive variable names.
colnames(X) <- c(features$V2, "subject", "activity")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
X_by_subject_activity <- aggregate(. ~ subject + activity, X, mean)

save(X_by_subject_activity, file = "result.RData")


