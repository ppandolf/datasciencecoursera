#################################################################
# run_analysis.R  compiles physical activity data collected 
# from the accelerometers from the Samsung Galaxy S smartphone 
# please see README and code book documentation for details
# this script is written to read input files from the
# /data/UCI HAR Dataset subdirectory within current working directory
#################################################################


library(dplyr)

##################################
# Read All Input Files
##################################
x_test_readings <- read.table("./data/UCI HAR Dataset/test/X_test.txt",colClasses = "numeric",comment.char = "")
y_test_activities <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
z_test_subjects <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

x_train_readings <- read.table("./data/UCI HAR Dataset/train/X_train.txt", colClasses = "numeric",comment.char = "")
y_train_activities <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
z_train_subjects <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt")

######################################################
# Assign Subject and Activity ID Variable Names  
######################################################
names(z_test_subjects)[1] <- "SubjectID"
names(z_train_subjects)[1] <- "SubjectID"
names(y_test_activities)[1] <- "ActivityID"
names(y_train_activities)[1] <- "ActivityID"

######################################################
# Add Train-Test class variable to Subject Data
######################################################
z_test_subjects <- mutate(z_test_subjects, SubjectClass = "Test")
z_train_subjects <- mutate(z_train_subjects, SubjectClass = "Train")

########################################################################
# Extract - Determine and Select Measurements to Keep For Analysis
# using features file row indexes. Keeping mean() and std(), NOT freqMean      
########################################################################
meanStdColumns <- grep("mean|std", features$V2, value = FALSE)
meanFreqColumns <- grep("meanFreq", features$V2, value = FALSE)

colstokeep <- meanStdColumns[!meanStdColumns %in% meanFreqColumns]
xtrct_test <- x_test_readings[,colstokeep]
xtrct_train <- x_train_readings[,colstokeep]

#######################################################################
# Combining Subject, Activity and Reading detail within each class First
# Then the Test and Training samples are (vertically) combined  
#######################################################################
xyz_test <- cbind(z_test_subjects, y_test_activities, xtrct_test)
xyz_train <- cbind(z_train_subjects, y_train_activities, xtrct_train)

merged_data <- rbind(xyz_test, xyz_train)


#######################################################################
# Original input and interim tables can be cleared now      
#######################################################################
# rm(x_test_readings, y_test_activities, z_test_subjects) 
# rm(x_train_readings, y_train_activities, z_train_subjects)
# rm(xtrct_test, xtrct_train)
# rm(xyz_test, xyz_train) 


#######################################################################
# Apply the Actvity labels   
#######################################################################
merged_data$ActivityID[merged_data$ActivityID == 1] <- as.character(activity_labels$V2[1])
merged_data$ActivityID[merged_data$ActivityID == 2] <- as.character(activity_labels$V2[2])
merged_data$ActivityID[merged_data$ActivityID == 3] <- as.character(activity_labels$V2[3])
merged_data$ActivityID[merged_data$ActivityID == 4] <- as.character(activity_labels$V2[4])
merged_data$ActivityID[merged_data$ActivityID == 5] <- as.character(activity_labels$V2[5])
merged_data$ActivityID[merged_data$ActivityID == 6] <- as.character(activity_labels$V2[6])


#######################################################################
# Correct and apply the measurement aka "features" names   
#######################################################################
features$V2 <- gsub("BodyBody", "Body", features$V2)
features$V2 <- make.names(features$V2, unique=TRUE, allow_=TRUE)

vnames <- (features$V2[colstokeep])
knames <- c("SubjectID" ,   "SubjectGroup" ,"Activity")

names(merged_data) <- c(knames, vnames)


#######################################################################
# Calculate arithmetic mean of each measurement variable 
# by subject-actvity combination   
#######################################################################
arrange(merged_data, Activity, SubjectID)

tdata <- summarize(grouped_data, 
 tBodyAcc.mean...X = mean(tBodyAcc.mean...X, na.rm = TRUE),
 tBodyAcc.mean...Y= mean(tBodyAcc.mean...Y, na.rm = TRUE),
 tBodyAcc.std...Z= mean(tBodyAcc.std...Z, na.rm = TRUE),
 tGravityAcc.std...X= mean(tGravityAcc.std...X, na.rm = TRUE),
 tBodyAccJerk.mean...Y= mean(tBodyAccJerk.mean...Y, na.rm = TRUE),
 tBodyAccJerk.std...Z= mean(tBodyAccJerk.std...Z, na.rm = TRUE),
 tBodyGyro.std...X= mean(tBodyGyro.std...X, na.rm = TRUE),
 tBodyGyroJerk.mean...Y= mean(tBodyGyroJerk.mean...Y, na.rm = TRUE),
 tBodyGyroJerk.std...Z= mean(tBodyGyroJerk.std...Z, na.rm = TRUE),
 tGravityAccMag.std..= mean(tGravityAccMag.std.., na.rm = TRUE),
 tBodyGyroMag.std..= mean(tBodyGyroMag.std.., na.rm = TRUE),
 fBodyAcc.mean...Y= mean(fBodyAcc.mean...Y, na.rm = TRUE),
 fBodyAcc.std...Z= mean(fBodyAcc.std...Z, na.rm = TRUE),
 fBodyAccJerk.std...X= mean(fBodyAccJerk.std...X, na.rm = TRUE),
 fBodyGyro.mean...Y= mean(fBodyGyro.mean...Y, na.rm = TRUE),
 fBodyGyro.std...Z= mean(fBodyGyro.std...Z, na.rm = TRUE),
 fBodyAccJerkMag.std..= mean(fBodyAccJerkMag.std.., na.rm = TRUE),
 fBodyGyroJerkMag.std..= mean(fBodyGyroJerkMag.std.., na.rm = TRUE),
 tBodyAcc.mean...Z= mean(tBodyAcc.mean...Z, na.rm = TRUE),
 tGravityAcc.mean...X= mean(tGravityAcc.mean...X, na.rm = TRUE),
 tGravityAcc.std...Y= mean(tGravityAcc.std...Y, na.rm = TRUE),
 tBodyAccJerk.mean...Z= mean(tBodyAccJerk.mean...Z, na.rm = TRUE),
 tBodyGyro.mean...X= mean(tBodyGyro.mean...X, na.rm = TRUE),
 tBodyGyro.std...Y= mean(tBodyGyro.std...Y, na.rm = TRUE),
 tBodyGyroJerk.mean...Z= mean(tBodyGyroJerk.mean...Z, na.rm = TRUE),
 tBodyAccMag.mean..= mean(tBodyAccMag.mean.., na.rm = TRUE),
 tBodyAccJerkMag.mean..= mean(tBodyAccJerkMag.mean.., na.rm = TRUE),
 tBodyGyroJerkMag.mean..= mean(tBodyGyroJerkMag.mean.., na.rm = TRUE),
 fBodyAcc.mean...Z= mean(fBodyAcc.mean...Z, na.rm = TRUE),
 fBodyAccJerk.mean...X= mean(fBodyAccJerk.mean...X, na.rm = TRUE),
 fBodyAccJerk.std...Y= mean(fBodyAccJerk.std...Y, na.rm = TRUE),
 fBodyGyro.mean...Z= mean(fBodyGyro.mean...Z, na.rm = TRUE),
 fBodyAccMag.mean..= mean(fBodyAccMag.mean.., na.rm = TRUE),
 fBodyGyroMag.mean..= mean(fBodyGyroMag.mean.., na.rm = TRUE),
 tBodyAcc.std...X= mean(tBodyAcc.std...X, na.rm = TRUE),
 tGravityAcc.mean...Y= mean(tGravityAcc.mean...Y, na.rm = TRUE),
 tGravityAcc.std...Z= mean(tGravityAcc.std...Z, na.rm = TRUE),
 tBodyAccJerk.std...X= mean(tBodyAccJerk.std...X, na.rm = TRUE),
 tBodyGyro.mean...Y= mean(tBodyGyro.mean...Y, na.rm = TRUE),
 tBodyGyro.std...Z= mean(tBodyGyro.std...Z, na.rm = TRUE),
 tBodyGyroJerk.std...X= mean(tBodyGyroJerk.std...X, na.rm = TRUE),
 tBodyAccMag.std..= mean(tBodyAccMag.std.., na.rm = TRUE),
 tBodyAccJerkMag.std..= mean(tBodyAccJerkMag.std.., na.rm = TRUE),
 tBodyGyroJerkMag.std..= mean(tBodyGyroJerkMag.std.., na.rm = TRUE),
 fBodyAcc.std...X= mean(fBodyAcc.std...X, na.rm = TRUE),
 fBodyAccJerk.mean...Y= mean(fBodyAccJerk.mean...Y, na.rm = TRUE),
 fBodyAccJerk.std...Z= mean(fBodyAccJerk.std...Z, na.rm = TRUE),
 fBodyGyro.std...X= mean(fBodyGyro.std...X, na.rm = TRUE),
 fBodyAccMag.std..= mean(fBodyAccMag.std.., na.rm = TRUE),
 fBodyGyroMag.std..= mean(fBodyGyroMag.std.., na.rm = TRUE),
 tBodyAcc.std...Y= mean(tBodyAcc.std...Y, na.rm = TRUE),
 tGravityAcc.mean...Z= mean(tGravityAcc.mean...Z, na.rm = TRUE),
 tBodyAccJerk.mean...X= mean(tBodyAccJerk.mean...X, na.rm = TRUE),
 tBodyAccJerk.std...Y= mean(tBodyAccJerk.std...Y, na.rm = TRUE),
 tBodyGyro.mean...Z= mean(tBodyGyro.mean...Z, na.rm = TRUE),
 tBodyGyroJerk.mean...X= mean(tBodyGyroJerk.mean...X, na.rm = TRUE),
 tBodyGyroJerk.std...Y= mean(tBodyGyroJerk.std...Y, na.rm = TRUE),
 tGravityAccMag.mean..= mean(tGravityAccMag.mean.., na.rm = TRUE),
 tBodyGyroMag.mean..= mean(tBodyGyroMag.mean.., na.rm = TRUE),
 fBodyAcc.mean...X= mean(fBodyAcc.mean...X, na.rm = TRUE),
 fBodyAcc.std...Y= mean(fBodyAcc.std...Y, na.rm = TRUE),
 fBodyAccJerk.mean...Z= mean(fBodyAccJerk.mean...Z, na.rm = TRUE),
 fBodyGyro.mean...X= mean(fBodyGyro.mean...X, na.rm = TRUE),
 fBodyGyro.std...Y= mean(fBodyGyro.std...Y, na.rm = TRUE),
 fBodyAccJerkMag.mean..= mean(fBodyAccJerkMag.mean.., na.rm = TRUE),
 fBodyGyroJerkMag.mean..= mean(fBodyGyroJerkMag.mean.., na.rm = TRUE))



#######################################################################
# Write out dataset for analysis 
# commented statements following the write can be used to 
# read the dataset back in from the /data subdirectory within 
# current working directory  
#######################################################################
write.table(tdata, file = "./data/getdata015_HARtidy.txt",row.name=FALSE) 

#mytidy <- read.table("./data/getdata015_HARtidy.txt")
#View(mytidy)



