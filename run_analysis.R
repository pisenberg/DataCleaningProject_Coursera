library(plyr)

## THis function loads the test and data training sets
## It is basically the main function to run in this script
loadData <- function() {
  
  #load the test data
  ########################################
  testData <- loadDataSubset("test")
  trainingData <- loadDataSubset("train")
  
  fullData <- rbind(testData,trainingData)
  
  #just for testing, look at the data check that we have 30 subjects
  print("")
  print("---------------------------------------------------------")
  print("Full Dataset")
  print("---------------------------------------------------------")
  print(paste("number of subjects: ",length(unique(fullData$subject_id))))
  print(paste("the dataset contains this many rows: ",nrow(fullData)))
  print(paste("the dataset contains this many columns: ",ncol(fullData)))
  
  createTidyData(fullData)
}

## This function is for step 5 of the project
## Here we create a smaller dataset where the average of each variable is calculated grouped by subject id and activity
createTidyData <-function(fullData){
  tinyData <- aggregate(fullData[,6:ncol(fullData)],by=list(fullData$subject_id,fullData$activity),FUN="mean")
  names(tinyData)[1] <- "Participant_ID"
  names(tinyData)[2] <- "Activity"
  #print(head(tinyData))
  write.table(tinyData,"TidyData_CourseProject.txt",row.name=FALSE)
}


## This function loads both the training and test data.
## It first takes a subject of X that is only mean and std values of different features
## and then merges this smaller dataset with the Y-set information
loadDataSubset <- function(experimentType){
  
  
  postFix <- "train"
  dir <- "UCI HAR Dataset/train/"
  
  if(experimentType=="test"){
    dir <-"UCI HAR Dataset/test/"
    postFix <- "test"
  }
  
  print(paste("currently loading dataset: ",postFix))
  
  ####LOAD THE SUBJECT AND WINDOW DATA
  ################################################################################
  subjectTestFile = paste(paste("subject_",postFix,sep=""),".txt",sep="")
  subjectWindows <- read.table(paste(dir,subjectTestFile,sep=""),colClasses = c("numeric"),col.names= c("subject_id"))
  
  #add an index for the windows
  indexsequence <- seq(1,nrow(subjectWindows))
  subjectWindows$window_id <- indexsequence
  
  #keep the info about which type of experiment this group belonged to, just in case
  subjectWindows$experiment_type <- rep(postFix,nrow(subjectWindows))
  
  #just for testing, look at the data check that we have 21 training and 9 test subjects
  
  print(paste("number of subjects: ",length(unique(subjectWindows$subject_id))))
  print(paste("the dataset contains this many windows: ",nrow(subjectWindows)))
  
  
  ####LOAD THE EXPERIMENT XY DATA
  ####This data is a 561-feature vector with time and frequency domain variables. 
  ################################################################################
  
  ##X Set
  xFile <- paste(paste("X_",postFix,sep=""),".txt",sep="")
  xSet <- read.table(paste(dir,xFile,sep=""),colClasses = rep("numeric",561))
  
  ##Now only get the xset data we need
  #doing this programmatically would be more clever
  xSubset <- data.frame(xSet[,1],xSet[,2],xSet[,3],
                        xSet[,4],xSet[,5],xSet[,6],
                        xSet[,41],xSet[,42],xSet[,43],
                        xSet[,44],xSet[,45],xSet[,46],
                        xSet[,81],xSet[,82],xSet[,83],
                        xSet[,84],xSet[,85],xSet[,86],
                        xSet[,121],xSet[,122],xSet[,123],
                        xSet[,124],xSet[,125],xSet[,126],
                        xSet[,161],xSet[,162],xSet[,163],
                        xSet[,164],xSet[,165],xSet[,166],
                        xSet[,201],
                        xSet[,202],
                        xSet[,214],
                        xSet[,215],
                        xSet[,227],
                        xSet[,228],
                        xSet[,240],
                        xSet[,241],
                        xSet[,253],
                        xSet[,254],
                        xSet[,266],xSet[,267],xSet[,268],
                        xSet[,269],xSet[,270],xSet[,271],
                        xSet[,294],xSet[,295],xSet[,296],
                        xSet[,345],xSet[,346],xSet[,347],
                        xSet[,348],xSet[,349],xSet[,350],
                        
                        xSet[,373],xSet[,374],xSet[,375],
                        
                        xSet[,424],xSet[,425],xSet[,426],
                        xSet[,427],xSet[,428],xSet[,429],
                        
                        xSet[,452],xSet[,453],xSet[,454],
                        
                        xSet[,503],
                        xSet[,504],
                        xSet[,513],
                        xSet[,516],
                        xSet[,517],
                        xSet[,526],
                        xSet[,529],
                        xSet[,530],
                        xSet[,539],
                        xSet[,542],
                        xSet[,543],
                        xSet[,552]
                        
                        )
  
  #print(ncol(xSubset))
    
  colnames(xSubset) <- c("tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z",
                         "tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z",
                         "tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z",
                          "tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z",
                          "tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z",
                          "tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z",
                          "tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z",
                          "tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z",
                          "tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z",
                          "tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z",
                          "tBodyAccMag-mean()",
                          "tBodyAccMag-std()",
                          "tGravityAccMag-mean()",
                          "tGravityAccMag-std()",
                          "tBodyAccJerkMag-mean()",
                          "tBodyAccJerkMag-std()",
                          "tBodyGyroMag-mean()",
                          "tBodyGyroMag-std()",
                          "tBodyGyroJerkMag-mean()",
                          "tBodyGyroJerkMag-std()",
                          "fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z",
                          "fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z",
                          "fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z",
                          "fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z",
                          "fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z",
                          "fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z",
                          "fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z",
                          "fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z",
                          "fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z",
                          "fBodyAccMag-mean()",
                          "fBodyAccMag-std()",
                          "fBodyAccMag-meanFreq()",
                          "fBodyBodyAccJerkMag-mean()",
                          "fBodyBodyAccJerkMag-std()",
                          "fBodyBodyAccJerkMag-meanFreq()",
                          "fBodyBodyGyroMag-meanFreq()",
                          "fBodyBodyGyroMag-mean()",
                          "fBodyBodyGyroMag-std()",
                          "fBodyBodyGyroJerkMag-mean()",
                          "fBodyBodyGyroJerkMag-std()",
                          "fBodyBodyGyroJerkMag-meanFreq()"
                         )
  
  ##Y Set
  yFile <- paste(paste("y_",postFix,sep=""),".txt",sep="")
  ySet <- read.table(paste(dir,yFile,sep=""),colClasses = c("numeric"),col.names = c("label"))
  
  #Label the activities properly
  ySet$activity[ySet$label==1] <- "WALKING"
  ySet$activity[ySet$label==2] <- "WALKING_UPSTAIRS"
  ySet$activity[ySet$label==3] <- "WALKING_DOWNSTAIRS"
  ySet$activity[ySet$label==4] <- "SITTING"
  ySet$activity[ySet$label==5] <- "STANDING"
  ySet$activity[ySet$label==6] <- "LAYING"
  
  #indexsequence <- seq(1,nrow(trainingYSet))
  #ySet$window_id <- indexsequence
  
  ##try to merge the x and y data into one data frame
  xySet <- cbind(ySet,xSubset)
  
  #print(head(fullSet))
  
  fullSet <- cbind(subjectWindows,xySet)

}



loadData()