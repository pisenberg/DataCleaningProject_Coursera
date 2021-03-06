DataCleaningProject_Coursera
============================

The code to run the project is contained in run_analysis.R.
Run the script or call load_data() directly.
The code is heavily documented to explain what is happening. In short:
* loadData calls loadDataSubset("test") and loadDataSubset("training").
* LoadDataSubset: 
  * This function loads both the training and test data. It first takes a subject of X that is only mean and std values of different features and then merges this smaller dataset with the Y-set information
* CreateTidyData:
  * This is the last function called with creates the final, smaller and tidy data file. This data file contains the following variables:
    * Participant ID - the ID of the participant in the study
	* Activity - a character string naming the activity the participant engaged in
	* A set of average measues for the following features:
	
> tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z",
> tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z",
> tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z",
> tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z",
> tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z",
> tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z",
> tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z",
> tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z",
> tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z",
> tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z",
> tBodyAccMag-mean()",
> tBodyAccMag-std()",
> tGravityAccMag-mean()",
> tGravityAccMag-std()",
> tBodyAccJerkMag-mean()",
> tBodyAccJerkMag-std()",
> tBodyGyroMag-mean()",
> tBodyGyroMag-std()",
> tBodyGyroJerkMag-mean()",
> tBodyGyroJerkMag-std()",
> fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z",
> fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z",
> fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z",
> fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z",
> fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z",
> fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z",
> fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z",
> fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z",
> fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z",
> fBodyAccMag-mean()",
> fBodyAccMag-std()",
> fBodyAccMag-meanFreq()",
> fBodyBodyAccJerkMag-mean()",
> fBodyBodyAccJerkMag-std()",
> fBodyBodyAccJerkMag-meanFreq()",
> fBodyBodyGyroMag-meanFreq()",
> fBodyBodyGyroMag-mean()",
> fBodyBodyGyroMag-std()",
> fBodyBodyGyroJerkMag-mean()",
> fBodyBodyGyroJerkMag-std()",
> fBodyBodyGyroJerkMag-meanFreq()"
)






