library(data.table)
library(dplyr)

##reading train data:
#reading features.txt into features
features<-read.csv("features.txt",header = F,sep=' ')
head(features)
#converting features into a character vector.
features <- as.character(features[,2])
#reading training set
trainset <- read.table('train/X_train.txt')
head(trainset)
#reading training activity labels
train_activity <- read.csv('train/y_train.txt', header = FALSE, sep = ' ')
head(train_activity)
#reading training subjects
train_subject <- read.csv('train/subject_train.txt',header = FALSE, sep = ' ')
head(train_subject)
#merging training sets data into train dataframe
train <-  data.frame(train_subject, train_activity, trainset)
head(train)
#naming columns of train df.
names(train) <- c(c('subject', 'activity'), features)
head(train)


##reading test data:
#reading test set
test_set <- read.table('test/X_test.txt')
head(test_set)
#reading test activity.
test_activity <- read.csv('test/y_test.txt', header = FALSE, sep = ' ')
head(test_activity)
#reading test subjects
test_subject <- read.csv('test/subject_test.txt', header = F,sep=' ')
head(test_subject)
#merging test data:
test <-  data.frame(test_subject, test_activity, test_set)
#naming colums of test data:
names(test) <- c(c('subject', 'activity'), features)
head(test)


##merging training and testing data to one dataset called data.all
data.all <- rbind(train, test)
head(data.all)

#Extracts only the measurements on the mean and standard deviation for each measurement into subset_data
mean_std.select <- grep('mean|std', names(data.all))
subset_data <- data.all[,c(1,2,mean_std.select)]

head(subset_data)

##3.Uses descriptive activity names to name the activities in the data set
#read activity_labels.txt into activity.labesl.
activity.labels <- read.csv('activity_labels.txt', header = FALSE,sep = " ")
head(activity.labels)
#get activitry labels as the character vector for corresponding values.
activity.labels <- as.character(activity.labels[,2])
#name activities in the dataset.
subset_data$activity <- activity.labels[subset_data$activity]
head(subset_data)

##4. Appropriately labels the data set with descriptive variable names.
temp_name <- names(subset_data)
#substituting () to ""
temp_name <- gsub("[(][)]", "", temp_name)
#substituting starting t to TimeDomain_
temp_name <- gsub("^t", "TimeDomain_", temp_name)
#substituting starting f to FrequencyDomain_
temp_name <- gsub("^f", "FrequencyDomain_", temp_name)
#substituting Acc to Accelerometer
temp_name <- gsub("Acc", "Accelerometer", temp_name)
#substituting Gyro to Gyroscope
temp_name <- gsub("Gyro", "Gyroscope", temp_name)
#substituting Mag to Magnitude
temp_name <- gsub("Mag", "Magnitude", temp_name)
#substituting -mean- to _Mean_
temp_name <- gsub("-mean-", "_Mean_", temp_name)
#substituting -std- to _standardDeviation_
temp_name <- gsub("-std-", "_StandardDeviation_", temp_name)
#substituting - to _
temp_name <- gsub("-", "_", temp_name)
#assigning names.new to names(subset_data)
names(subset_data) <- temp_name

head(subset_data)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#applying mean to every variable for activity and subject using aggregate and storing in tidy data.
tidydata <- aggregate(subset_data[,3:81], by = list(activity = subset_data$activity, subject = subset_data$subject),FUN = mean)
head(tidydata)
write.table(tidydata, file = "tidydata.txt", row.names = FALSE)
