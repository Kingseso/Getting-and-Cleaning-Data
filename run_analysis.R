#############################################################################
#file:  run_analysis.R
#Author:  Cecil Rivers
#Course:  Getting and Cleaning Data
#############################################################################

#############################################################################
#This program reads the test and training data from the UCI Machine Learning Repository 
#and puts a subset of the data in a tidy dataset.

setwd("~/Coursera/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

#Read files
test_data <- read.table("./test/X_test.txt")    #Read test data from X_test.txt file
test_labels <- read.table("./test/Y_test.txt")  #Read test labels from Y_test.txt file

train_data <- read.table("./train/X_train.txt")    #Read test data from X_train.txt file
train_labels <- read.table("./train/Y_train.txt")  #Read test labels from Y_train.txt file

features <- read.table("./features.txt")    #Read feature.txt file
activity_labels <- read.table("./activity_labels.txt")  #Read activity labels file
colnames(activity_labels) <- c("id","description")      #give activity labels variable names

#Add labels to train and test dataset
train <- cbind(train_labels,train_data)
test <- cbind(test_labels,test_data)

# #Change first column label name to "labels"
colnames(train)[1] <- "labels"
colnames(test)[1] <- "labels"

#Merge test and train data sets
merged_data <- merge(train,test,by=intersect(names(train),names(test)),all=TRUE)

#change the merged data column names to the names found in features.txt
for (i in 1:length(features[,2])){
    colnames(merged_data)[i+1] <- as.character(features[i,2])
}

#Extract mean and standard deviation data
mean_sd_data <- merged_data[,grepl("\\<mean()\\>",names(merged_data)) | grepl("\\<std()\\>",names(merged_data))]    #extract wildcard mean() or std()
mean_sd_data <- cbind(merged_data$labels,mean_sd_data)  #add back in the label numbers with the observations
colnames(mean_sd_data)[1] <- "labels"   #change the column name for the label numbers column

#Replace activity label descriptions in mean/std dataset
mean_sd_data$labels <- activity_labels$description[mean_sd_data$labels]

result<-data.frame(matrix(nrow=0,ncol=length(mean_sd_data)))    #create an empty dataset with same number of columns as mean/std dataset
colnames(result) <- names(mean_sd_data)                         #copy over the dataset variable names

#Calculate column means for each activity label
for (i in 1:length(activity_labels[,1])){                   
    x <- subset(mean_sd_data,activity_labels[i,2] == mean_sd_data$labels)   #subset activity labels from mean/std dataset 
    y <- as.data.frame(t(colMeans(x[,-1])))                                 #calculate column means (except for first column)
    result <- rbind(result,cbind(activity_labels[i,2],y))                   #column combine column mean for the subset activity label to the result dataset
}
colnames(result)[1] <- "labels"     #name first column "labels" in result dataset

write.table(result,file="tidy_result.txt",row.names=FALSE)    #write result dataset to a text file (removing the row names)