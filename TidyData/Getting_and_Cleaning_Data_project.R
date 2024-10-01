
# Read the 'features.txt' file into a data frame called 'features'
features <- read.table('UCI_HAR_Dataset\\features.txt')

# Read the 'X_train.txt' file into a data frame called 'X_train' (this is the actual dataset with observations)
X_train <- read.table('UCI_HAR_Dataset\\train\\X_train.txt')

# Set the correct columns names from the features file
colnames(X_train) <- features[,2]

# Read the 'Y_train.txt' file into a data frame (this holds the activity labels)
Y_train <- read.table('UCI_HAR_Dataset\\train\\Y_train.txt')
colnames(Y_train) <- 'Labels' # seting the correct column name

# Combine the 'Y_train' data frame (activity labels) with the 'X_train' data frame (sensor measurements)
# This adds the 'Labels' column to the front of 'X_train', aligning the activity labels with the sensor data
X_train <- cbind(Y_train, X_train)

# Add the Subject_ID column  
subjects_train <- read.table('UCI_HAR_Dataset\\train\\subject_train.txt')
colnames(subjects_train) <- 'Subject_ID'
X_train <- cbind(subjects_train, X_train)

# Read the 'X_test.txt' file 
X_test <- read.table('UCI_HAR_Dataset\\test\\X_test.txt')

# Set the correct columns names from the features file
colnames(X_test) <- features[,2]

# Read the 'Y_test.txt' file into a data frame called 'labls' (this holds the activity labels)
Y_test <- read.table('UCI_HAR_Dataset\\test\\Y_test.txt')
colnames(Y_test) <- 'Labels' # seting the correct column name

# Combine the X_test with its Labels
X_test <- cbind(Y_test, X_test)

# Add the Subject_ID column  
subjects_test <- read.table('UCI_HAR_Dataset\\test\\subject_test.txt')
colnames(subjects_test) <- 'Subject_ID'
X_test <- cbind(subjects_test, X_test)

# Combine the two data frames
complet_DF <- rbind.data.frame(X_train, X_test)


# Capture the names of the columns mesuring the mean and standard deviation
mean_std_indices <- grep("mean\\(\\)|std\\(\\)", colnames(complet_DF))

# Broadcasting the captured names of interest
complet_DF <- complet_DF[, c(1, 2, mean_std_indices)]

# Changing the Labels values to descritives ones
# create a mpping vector
activity_labels <- read.table('UCI_HAR_Dataset\\activity_labels.txt')
label_maping <- (activity_labels[,2])
names(label_maping) <- activity_labels[,1]

# Map the Labels codes to its respectevie description
complet_DF$Labels <- label_maping[complet_DF$Labels]

# label the data with descriptive variables names
# descriptive names
new_names <- c(
  "Subject_ID", "Activity_Label", 
  "TimeBodyAccelerometerMean-X", "TimeBodyAccelerometerMean-Y", "TimeBodyAccelerometerMean-Z", 
  "TimeBodyAccelerometerStd-X", "TimeBodyAccelerometerStd-Y", "TimeBodyAccelerometerStd-Z", 
  "TimeGravityAccelerometerMean-X", "TimeGravityAccelerometerMean-Y", "TimeGravityAccelerometerMean-Z", 
  "TimeGravityAccelerometerStd-X", "TimeGravityAccelerometerStd-Y", "TimeGravityAccelerometerStd-Z", 
  "TimeBodyAccelerometerJerkMean-X", "TimeBodyAccelerometerJerkMean-Y", "TimeBodyAccelerometerJerkMean-Z", 
  "TimeBodyAccelerometerJerkStd-X", "TimeBodyAccelerometerJerkStd-Y", "TimeBodyAccelerometerJerkStd-Z", 
  "TimeBodyGyroscopeMean-X", "TimeBodyGyroscopeMean-Y", "TimeBodyGyroscopeMean-Z", 
  "TimeBodyGyroscopeStd-X", "TimeBodyGyroscopeStd-Y", "TimeBodyGyroscopeStd-Z", 
  "TimeBodyGyroscopeJerkMean-X", "TimeBodyGyroscopeJerkMean-Y", "TimeBodyGyroscopeJerkMean-Z", 
  "TimeBodyGyroscopeJerkStd-X", "TimeBodyGyroscopeJerkStd-Y", "TimeBodyGyroscopeJerkStd-Z", 
  "TimeBodyAccelerometerMagnitudeMean", "TimeBodyAccelerometerMagnitudeStd", 
  "TimeGravityAccelerometerMagnitudeMean", "TimeGravityAccelerometerMagnitudeStd", 
  "TimeBodyAccelerometerJerkMagnitudeMean", "TimeBodyAccelerometerJerkMagnitudeStd", 
  "TimeBodyGyroscopeMagnitudeMean", "TimeBodyGyroscopeMagnitudeStd", 
  "TimeBodyGyroscopeJerkMagnitudeMean", "TimeBodyGyroscopeJerkMagnitudeStd", 
  "FrequencyBodyAccelerometerMean-X", "FrequencyBodyAccelerometerMean-Y", "FrequencyBodyAccelerometerMean-Z", 
  "FrequencyBodyAccelerometerStd-X", "FrequencyBodyAccelerometerStd-Y", "FrequencyBodyAccelerometerStd-Z", 
  "FrequencyBodyAccelerometerJerkMean-X", "FrequencyBodyAccelerometerJerkMean-Y", "FrequencyBodyAccelerometerJerkMean-Z", 
  "FrequencyBodyAccelerometerJerkStd-X", "FrequencyBodyAccelerometerJerkStd-Y", "FrequencyBodyAccelerometerJerkStd-Z", 
  "FrequencyBodyGyroscopeMean-X", "FrequencyBodyGyroscopeMean-Y", "FrequencyBodyGyroscopeMean-Z", 
  "FrequencyBodyGyroscopeStd-X", "FrequencyBodyGyroscopeStd-Y", "FrequencyBodyGyroscopeStd-Z", 
  "FrequencyBodyAccelerometerMagnitudeMean", "FrequencyBodyAccelerometerMagnitudeStd", 
  "FrequencyBodyBodyAccelerometerJerkMagnitudeMean", "FrequencyBodyBodyAccelerometerJerkMagnitudeStd", 
  "FrequencyBodyBodyGyroscopeMagnitudeMean", "FrequencyBodyBodyGyroscopeMagnitudeStd", 
  "FrequencyBodyBodyGyroscopeJerkMagnitudeMean", "FrequencyBodyBodyGyroscopeJerkMagnitudeStd"
)

# Assigning new names to the dataframe
colnames(complet_DF) <- new_names

#summarizing the data set
# Load the dplyr package
library(dplyr)

# Group by "Activity_Label" and "Subject_ID", then calculate the average of each variable using the updated syntax
summary_df <- complet_DF %>%
  group_by(Activity_Label, Subject_ID) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = "drop")


write.table(summary_df, file = "tidy_dataset.txt", row.names = FALSE)

