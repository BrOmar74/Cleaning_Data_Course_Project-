library(dplyr)
library(plyr)

#Creates a directory to store the data if it doesn't already exist
if (!file.exists("./data")){dir.create('./data')}

#Downloads data...and store it into a temporary file before unzipping it 
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileURL,temp)
unzip(temp,exdir = "./data")
unlink(temp)


#loading the X_train,X_test, Y_train and Y_test assuming that we do not need the raw data in the intertial signals forlder

X_train_set <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
X_test_set <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
Y_train_set <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
Y_test_set <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")

#First step: Merging the train and test sets X and Y )

X_merged_data <- rbind(X_train_set,X_test_set)
Y_merged_data <- rbind(Y_train_set,Y_test_set)

merged_data <- cbind(X_merged_data,Y_merged_data)

#Second step: retaining only the mean and std assuming that we want any variable that contains "mean" or "std"
features <- read.table("./data/UCI HAR Dataset/features.txt")
features_logical <- grepl("mean",features[,2]) | grepl("std",features[,2])
# we get the indices and the names for selecting and describing the columns later on
features_index <- features[features_logical,1]
features_names <- features[features_logical,2]

features_names <- sub("-","_",sub("-","_",features_names))

X_merged_data_filtered <- X_merged_data[,features_index]

#Third step: activity_labels, we use the file that has the activity labels to map the values to the STANDING, SITTING...
#and create a factor of labels

activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
Y_factor <- mapvalues(factor(Y_merged_data$V1), from = activity_labels[,1], to = activity_labels[,2])


#we add the factor vector to the data frame
XY_merged_data <- cbind(X_merged_data_filtered,Y_factor)

#Fourth step: adding descriptive names for each variable of the data set, we get this from the file features.txt

names(XY_merged_data) <- c(features_names,"activity_labels")

#5th Step: creating a second, independent tidy data set with the average of each variable for each activity and each subject.

subject_train_id <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subject_test_id <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#adding the subject ids
XY_merged_data$subjectID <-  rbind(subject_train_id,subject_test_id)

#computing the mean for each subject and activity
tidy_data_set <- XY_merged_data %>% group_by(subjectID,activity_labels) %>% summarize_all(mean) %>% as.data.frame()


#some arrangements before writing to have a clean data frame, setting factors as character and converting subjectID into a vector (before it was a data frame)
tidy_data_set[,1] <- tidy_data_set$subjectID[,1]
tidy_data_set[,2] <- as.character(tidy_data_set$activity_labels)


write.table(tidy_data_set, "WideTidyData.txt",row.names = FALSE,sep = '\t')
