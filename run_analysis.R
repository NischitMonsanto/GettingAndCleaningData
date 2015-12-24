
#1. Merges the training and the test sets to create one data set.
#Read and merge X data
x_train <- read.table("UCI HAR Dataset\\train\\X_train.txt", header = FALSE)
x_test <- read.table("UCI HAR Dataset\\test\\X_test.txt", header = FALSE)
x_merge <- rbind(x_train, x_test)

#Read and merge Y data
y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt", header = FALSE)
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt", header = FALSE)
y_merge <- rbind(y_train, y_test)

#Read and merge subject data
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt", header = FALSE)
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt", header = FALSE)
subject_merge <- rbind(subject_train,subject_test)

colnames(x_merge)
colnames(y_merge)
colnames(subject_merge)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#Read features.txt and give columns names to x_merge
features <- read.table("UCI HAR Dataset\\features.txt", header = FALSE)
names(x_merge) <- features[,2]

#Give name for y_merge
names(y_merge) <- "activities"

#Give name for subject_merge
names(subject_merge) <- "subject"

#bind all data columnwise
full_merge <- cbind(x_merge, y_merge, subject_merge)

full_mean <- grep(".*mean.*", colnames(full_merge), ignore.case = TRUE)
full_std <- grep(".*std.*", colnames(full_merge), ignore.case = TRUE)
#Also add 562 and 563 for activities and subject
full_mean_std <- c(full_mean, full_std, 562, 563)

mean_std_merge <- full_merge[,full_mean_std]

#Read activity_labels.txt and give columns names to x_merge
activities <- read.table("UCI HAR Dataset\\activity_labels.txt", header = FALSE)

#3. Uses descriptive activity names to name the activities in the data set
mean_std_merge$activities<-as.character(mean_std_merge$activities)
for (i in 1:6){
  mean_std_merge$activities[mean_std_merge$activities==i]<-as.character(activities[i,2])
}


#4. Appropriately labels the data set with descriptive variable names. 
names(mean_std_merge)<-gsub("-mean()", "Mean", names(mean_std_merge), ignore.case = TRUE)
names(mean_std_merge)<-gsub("-std()", "STD", names(mean_std_merge), ignore.case = TRUE)
names(mean_std_merge)<-gsub("()", "", names(mean_std_merge))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_std_merge$activities <- as.factor(mean_std_merge$activities)
mean_std_merge$subject <- as.factor(mean_std_merge$subject)
mean_std_merge<-data.table(mean_std_merge)
new_tidy_data <- aggregate(. ~ activities + subject, data = mean_std_merge, mean)

#Write into file
write.table(new_tidy_data, file = "tidy_data_set_1.txt", sep = ",", row.name=FALSE, col.names = colnames(new_tidy_data), qmethod = "double")



