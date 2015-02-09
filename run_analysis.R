## 1. Merging the data.
# Load data necessary to compile the total testing data.
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt") # Column of test subject IDs
values_test <- read.table("./UCI HAR Dataset/test/X_test.txt") # Values for all measurements
activities_test <- read.table("./UCI HAR Dataset/test/y_test.txt") # Column of activity labels

# Compile the total testing data.
test_data <- cbind(subjects_test, activities_test, values_test)

# Load data necessary to compile the total training data.
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt") # Column of training subject IDs
values_train <- read.table("./UCI HAR Dataset/train/X_train.txt") # Values for all measurements
activities_train <- read.table("./UCI HAR Dataset/train/y_train.txt") # Column of activity labels

# Compile the total training data.
train_data <- cbind(subjects_train, activities_train, values_train)

# Merge the training and testing data.
total_data <- rbind(test_data, train_data)


## 2. Extracting only the mean and SD measurements.
# Read in features.txt for the measurement descriptions.
measurements <- read.table("./UCI HAR Dataset/features.txt")

#Find the mean and SD measurement column numbers (Added 2 for subject ID and activity columns).
keep_columns <- (grep("-mean[^F]|-std", measurements$V2)+2)

#Select only the desired columns.
sub_data <- total_data[,c(1,2,keep_columns)]


## 3. Name the activities.
# Function to convert the coded activity integer to a descriptive name.
activity_name <- function(x) {
        if (x==1) {x <- "walking"}
        else if (x==2) {x <- "walkingupstairs"}
        else if (x==3) {x <- "walkingdownstairs"}
        else if (x==4) {x <- "sitting"}
        else if (x==5) {x <- "standing"}
        else if (x==6) {x <- "laying"}
}

#Converts activity codes to words.
sub_data$V1.1 <- sapply(sub_data$V1.1, activity_name)


## 4. Name the variables.
# List of kept measurement names from features.txt.
measure_names <- grep("-mean[^F]|-std", measurements$V2, value = TRUE)

# Convert the list of variable names to a tidy format.
measure_names <- tolower(measure_names)
measure_names <- gsub("-","",measure_names)
measure_names <- gsub("[()]","",measure_names)

# Compile a final variable list and update the variable names.
variable_list <- c("id","activity",measure_names)
names(sub_data) <- variable_list


## 5. Create a second data set with the average of each variable for each subject and activity combination.
# Group data by subject and activity.
library(dplyr)
grouped_sub_data <- group_by(sub_data,id,activity)

# Summarise each measurement's mean by groups.
summarised_sub_data <- summarise_each(grouped_sub_data, funs(mean))

# Return the summarised data set as a text file.
write.table(summarised_sub_data, file = "./course project.txt", row.names=FALSE)