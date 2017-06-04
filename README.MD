Getting and cleaning data

For creating a tidy data set of wearable computing data originally from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Files in this repo

README.md -- you are reading it right now
CodeBook.md -- codebook describing variables, the data and transformations
run_analysis.R -- actual R code
run_analysis.R goals

You should create one R script called run_analysis.R that does the following:

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive activity names.
Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
It should run in a folder of the Samsung data (the zip had this folder: UCI HAR Dataset) The script assumes it has in it's working directory the following files and folders:

activity_labels.txt
features.txt
test/
train/
The output is created in working directory with the name of mytidy.txt

run_analysis.R walkthrough

It follows the goals step by step.

Step 1:

Download the file to local and Unzip it.

Step 2:

Set the working directory to the folder where files reside.

Step 3:

Read all the test and training files: y_test.txt, subject_test.txt and X_test.txt.

Assign the column names to the data.

Combine the files to a data frame in the form of subjects, labels, the rest of the data.

Create vector for column names to be used further.


Step 4:

Extract only the measurements on the mean and standard deviation for each measurement for the subId and ActivityId.

Step 5:

Join the combined data set with only the measurements on the mean and standard deviation for each measurement with activityLabel data set.Used descriptive activity names to name the activities in the data set.

Removed column ActivityId

Step 6:

Appropriately labels the data set with descriptive variable names.Remove parentheses,correct syntax in names and add descriptive names for the column names.

Step 7:

Create a new data frame by finding the mean for each combination of subject and Activity. It's done by aggregate() function
Final step:

Write the new tidy set into a text file called mytidy.txt, formatted similarly to the original files.