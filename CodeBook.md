
Code Book
==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - UniversitÓ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.



Variables
========

subject                    1..2
    Subject number
                           1..30 .Unique identifier assigned to each subject

label                      6..18
    Acitivity label
                           "WALKING"
                           "WALKING_UPSTAIRS"
                           "WALKING_DOWNSTAIRS"
                           "SITTING"
                           "STANDING"
                           "LAYING"

tbodyacc-mean-x	
Mean time for acceleration of body for X direction

tbodyacc-mean-y	
Mean time for acceleration of body for Y direction

tbodyacc-mean-z	
Mean time for acceleration of body for Z direction

tbodyacc-std-x	
Standard deviation of time for acceleration of body for X direction

tbodyacc-std-y	
Standard deviation of time for acceleration of body for Y direction

tbodyacc-std-z	
Standard deviation of time for acceleration of body for Z direction

tgravityacc-mean-x	
Mean time of acceleration of gravity for X direction

tgravityacc-mean-y	
Mean time of acceleration of gravity for Y direction

tgravityacc-mean-z	
Mean time of acceleration of gravity for Z direction

tgravityacc-std-x	
Standard deviation of time of acceleration of gravity for X direction

tgravityacc-std-y	
Standard deviation of time of acceleration of gravity for Y direction

tgravityacc-std-z	
Standard deviation of time of acceleration of gravity for Z direction

tbodyaccjerk-mean-x	
Mean time of body acceleration jerk for X direction

tbodyaccjerk-mean-y	
Mean time of body acceleration jerk for Y direction

tbodyaccjerk-mean-z	
Mean time of body acceleration jerk for Z direction

tbodyaccjerk-std-x	
Standard deviation of time of body acceleration jerk for X direction

tbodyaccjerk-std-y	
Standard deviation of time of body acceleration jerk for Y direction

tbodyaccjerk-std-z	
Standard deviation of time of body acceleration jerk for Z direction

tbodygyro-mean-x	
Mean body gyroscope measurement for X direction

tbodygyro-mean-y	
Mean body gyroscope measurement for Y direction

tbodygyro-mean-z	
Mean body gyroscope measurement for Z direction

tbodygyro-std-x	
Standard deviation of body gyroscope measurement for X direction

tbodygyro-std-y	
Standard deviation of body gyroscope measurement for Y direction

tbodygyro-std-z	
Standard deviation of body gyroscope measurement for Z direction

tbodygyrojerk-mean-x	
Mean jerk signal of body for X direction

tbodygyrojerk-mean-y	
Mean jerk signal of body for Y direction

tbodygyrojerk-mean-z	
Mean jerk signal of body for Z direction

tbodygyrojerk-std-x	
Standard deviation of jerk signal of body for X direction

tbodygyrojerk-std-y	
Standard deviation of jerk signal of body for Y direction

tbodygyrojerk-std-z	
Standard deviation of jerk signal of body for Z direction

tbodyaccmag-mean	
Mean magnitude of body Acc

tbodyaccmag-std	
Standard deviation of magnitude of body Acc

tgravityaccmag-mean	
Mean gravity acceleration magnitude

tgravityaccmag-std	
Standard deviation of gravity acceleration magnitude

tbodyaccjerkmag-mean	
Mean magnitude of body acceleration jerk

tbodyaccjerkmag-std	
Standard deviation of magnitude of body acceleration jerk

tbodygyromag-mean	
Mean magnitude of body gyroscope measurement

tbodygyromag-std	
Standard deviation of magnitude of body gyroscope measurement

tbodygyrojerkmag-mean	
Mean magnitude of body body gyroscope jerk measurement

tbodygyrojerkmag-std	
Standard deviation of magnitude of body body gyroscope jerk measurement

fbodyacc-mean-x	
Mean frequency of body acceleration for X direction

fbodyacc-mean-y	
Mean frequency of body acceleration for Y direction

fbodyacc-mean-z	
Mean frequency of body acceleration for Z direction

fbodyacc-std-x	
Standard deviation of frequency of body acceleration for X direction

fbodyacc-std-y	
Standard deviation of frequency of body acceleration for Y direction

fbodyacc-std-z	
Standard deviation of frequency of body acceleration for Z direction

fbodyaccjerk-mean-x	
Mean frequency of body accerlation jerk for X direction

fbodyaccjerk-mean-y	
Mean frequency of body accerlation jerk for Y direction

fbodyaccjerk-mean-z	
Mean frequency of body accerlation jerk for Z direction

fbodyaccjerk-std-x	
Standard deviation frequency of body accerlation jerk for X direction

fbodyaccjerk-std-y	
Standard deviation frequency of body accerlation jerk for Y direction

fbodyaccjerk-std-z	
Standard deviation frequency of body accerlation jerk for Z direction

fbodygyro-mean-x	
Mean frequency of body gyroscope measurement for X direction

fbodygyro-mean-y	
Mean frequency of body gyroscope measurement for Y direction

fbodygyro-mean-z	
Mean frequency of body gyroscope measurement for Z direction

fbodygyro-std-x	
Standard deviation frequency of body gyroscope measurement for X direction

fbodygyro-std-y	
Standard deviation frequency of body gyroscope measurement for Y direction

fbodygyro-std-z	
Standard deviation frequency of body gyroscope measurement for Z direction

fbodyaccmag-mean	
Mean frequency of body acceleration magnitude

fbodyaccmag-std	
Standard deviation of frequency of body acceleration magnitude

fbodybodyaccjerkmag-mean	
Mean frequency of body acceleration jerk magnitude

fbodybodyaccjerkmag-std	
Standard deviation of frequency of body acceleration jerk magnitude

fbodybodygyromag-mean	
Mean frequency of magnitude of body gyroscope measurement

fbodybodygyromag-std	
Standard deviation of frequency of magnitude of body gyroscope measurement

fbodybodygyrojerkmag-mean	
Mean frequency of magnitude of body gyroscope jerk measurement

fbodybodygyrojerkmag-std	
Standard deviation frequency of magnitude of body gyroscope jerk measurement


The set of variables that were estimated from these signals are:

mean: Mean value

std: Standard deviation

Transformation
================

All the values are means, aggregated over 30 subjects and 6 activities, hence the resulting dataset is 180 rows by 68 columns.