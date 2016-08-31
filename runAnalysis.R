library('dplyr')
library(data.table)
source('general.R')

# This script (run_analysis.R) does the following:
# 
# 1. Merges the training and the test sets to create one data set.
#    - should be ~ 10k records
#    - load X_train.txt and X_test.txt
# 2. Extracts only the measurements on the mean and standard deviation for each measurement \
#    - presumed that a 'measurement' is a record line accross subject, X, y
#    - use features.txt to find mean/std dev
#      - filter to std/mean
#      - find indices
#    - think how to demonstrate that the data is clean
# 3. Uses descriptive activity names to name the activities in the data set
#    - reads and map activities.txt
#    - y_labels contain the indices to the activites in activities.txt
# 4. Appropriately labels the data set with descriptive variable names.
#    - Activity, Subject #, data points
#    - Did not de-compose the column labels since this is embedded information
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#    - Activity, Subject #, data points (avg)
#    - take activity, subject, find averages for combination of activity, subjects
#    - should be 30 subjects * 6 activities, or 180 records in length
#


# The purpose of this routine is to read data from a test/train data folder with the following structure:
# ./
# ./X_file_suffix.txt
# ./y_file_suffix.txt
# ./subject_file_suffix.txt
process_dir <- function(file_suffix) {
  pushd(file_suffix)
    
    #generate label, subject, and sensor data file names
    label_file_name <- paste('y_',file_suffix,'.txt',sep='')
    subject_file_name <- paste('subject_',file_suffix,'.txt',sep='')
    sensor_data_file_name <- paste('X_',file_suffix,'.txt',sep='')
    
    #labels
    ylabel_indices <- data.table(read.table(label_file_name,col.names = c('TIndex'))) %>%
      # Add textual labels - this will work because the index is the same as the position
      mutate(Activity=activity_lbls[TIndex]$Activity) %>%
      # Remove indices information
      mutate(TIndex=NULL)
    
    #subjects
    subject_numbers <- data.table(read.table(subject_file_name,col.names = c('Subject')))
    
    #raw sensor data
    sensor_data <- read.table(sensor_data_file_name,col.names=feat_data$Measurement)
    #retrieve the set of indices we care about (those that have std/mean in them)
    actual_data <- data.table(sensor_data[filter_cols_measure_data$Index])
    
    #combine
    actual_data <- cbind(ylabel_indices,subject_numbers,actual_data)
    
    
  popd()
  
  #cleanup
  remove(label_file_name,subject_file_name,sensor_data_file_name)
  remove(sensor_data,subject_numbers,ylabel_indices)
  
  #return the data
  actual_data
}

#set everything relative to the directory we will be working in
setwd('~/Documents/coursera/data-cleaning/04_week-4/')

#Enter our data directory
pushd('UCI HAR Dataset')
  
  #load activity labels
  activity_lbls <- data.table(read.table("activity_labels.txt",sep=' ',col.names=c("Index","Activity")))
  feat_data <- data.table(read.table('features.txt',col.names=c('Index','Measurement')))
  
  #only load things to do with std or mean
  filter_cols_measure_data <- filter(feat_data,grepl("std",Measurement) | grepl("mean",Measurement))
  
  #reference both test and train data...
  folders <- c('test','train')
  
  #merge the data from test/train folders
  results <- lapply(folders,process_dir)
  merged_data <- rbindlist(results)
  
  print('Dimensions of merged data:')
  print(dim(merged_data))
  
  print('Names of columns of merged data:')
  print(names(merged_data))
  
  res <- group_by(merged_data,Activity,Subject) %>%
  summarize_each(funs(mean),-Activity,-Subject)
  

popd()

#write our results
write.table(res,file='averages.txt',row.names = FALSE)

#cleanup
#remove(res,merged_data)
