# get the unique indices of features that have the words
# std or mean in them. Ignore case.
uniqueStdMeanIndices <- function() {
  df_features <-  read.delim("features.txt" , sep = "", header = FALSE) 
  names(df_features) <- c("id", "feature")

  # grep function returns the indices 
  output_indices <- unique(c(
    grep("std", df_features$feature, ignore.case = TRUE) , 
    grep("mean", df_features$feature, ignore.case = TRUE) 
  ))

  return(output_indices)
}

# This function is responsible for the following:
# - Merges the training and the test sets to create one data set.
# - Extracts only the measurements on the mean and standard deviation for each measurement. 
# - Uses descriptive activity names to name the activities in the data set
# - Appropriately labels the data set with descriptive variable names. 
# - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

run_analysis <- function(printOutput = FALSE) {
  
  #get all file names
  x_train <- "./train/X_train.txt"
  x_test <- "./test/X_test.txt"
  y_train <- "./train/y_train.txt"
  y_test <- "./test/y_test.txt"
  subject_train <-  "./train/subject_train.txt"
  subject_test <- "./test/subject_test.txt"
  
  
  # make data frames
  df_x_train <- read.delim(x_train, sep = "", header = FALSE)
  df_x_test <- read.delim(x_test, sep = "", header = FALSE)
  df_y_train <- read.delim(y_train, sep = "", header = FALSE)
  df_y_test <- read.delim(y_test, sep = "", header = FALSE)
  df_subject_train <- read.delim(subject_train, sep = "", header = FALSE)
  df_subject_test <- read.delim(subject_test, sep = "", header = FALSE)
  
  # merge train and test files
  
  #X value merge , add descriptive variable names
  df_x <- rbind(df_x_train, df_x_test)
  df_features <-  read.delim("features.txt" , sep = "", header = FALSE) 
  names(df_features) <- c("id", "feature")
  names(df_x) <- df_features$feature
  #  filter x to only contain mean and std columns
  df_x_mean_std <- df_x[, uniqueStdMeanIndices()] # ----------------(1)
  
  #y value merge , add activity name and also descriptive variable names
  df_y <- rbind(df_y_train, df_y_test)
  names(df_y) <- "activityId"
  #Uget the names corresponding to activity ids
  df_activities <- read.delim("activity_labels.txt" , sep = "", header = FALSE)
  names(df_activities) <- c("activityId", "activityName")
  row.names(df_activities) <- df_activities$activityId
  df_y_mutated <- mutate(df_y, activityName = df_activities[activityId, ]$activityName) # ----(2)
  
  #subject value merge , add descriptive variable name
  df_subject <- rbind(df_subject_train, df_subject_test) # ----(3)
  names(df_subject) <- "subjectId"
  
  #merge into one big data frame
  df_result <- cbind(df_subject,df_y_mutated, df_x_mean_std ) # ----(4)
  
  #independent tidy data set with the average of each variable for each activity
  # group by activity name
  df_result_gb_activity <- group_by(df_result, activityName)
  
  # convert to character vector
  col_names <- as.character(df_features$feature[uniqueStdMeanIndices()])
  # summarize onl numeric columns
  df_result_mean_activity <- summarise_at(df_result_gb_activity,col_names, mean)

  #independent tidy data set with the average of each variable for each subject
  # group by subject id
  df_result_gb_subject <- group_by(df_result, subjectId)
  # summarize onl numeric columns
  df_result_mean_subject <- summarise_at(df_result_gb_subject,col_names, mean)
 
  
   # row bind the activity and subject data and write  the data to a table
  colnames(df_result_mean_activity)[1] <- "name"
  colnames(df_result_mean_subject)[1] <- "name"
  df_result_mean_activity$name <- as.character(df_result_mean_activity$name)
  df_result_mean_subject$name <- as.character(df_result_mean_subject$name)
  print(sapply(df_result_mean_activity[,1:3],class))
  print(sapply(df_result_mean_subject[,1:3],class))
  write.table(rbind(df_result_mean_activity, df_result_mean_subject), "./mean_activity_subject.txt")
  write.table(df_result_mean_subject, "./mean_subject.txt")

  # Testing
  if(printOutput){
    # print(dim(df_x_train))
    # print(dim(df_x_test))
    # print(dim(df_x))
    print(dim(df_x_mean_std))
    #print(head(df_x_mean_std))
    
    # print(dim(df_y_train))
    # print(dim(df_y_test))
    # print(dim(df_y))
    # print(head(df_y))
    print(dim(df_y_mutated))
    #print(head(df_y_mutated))
    
    # print(dim(df_subject_train))
    # print(dim(df_subject_test))
    print(dim(df_subject))
    #print(head(df_subject))
    
    print(dim(df_result))
    print(dim(df_result_mean_activity))
    print(dim(df_result_mean_subject))
    
  }
  
}