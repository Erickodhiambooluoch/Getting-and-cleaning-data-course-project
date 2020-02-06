getwd()
# Download data from the internet 
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data1 <- download.file(fileurl, destfile = "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
# confirm if the downloaded files are actually in the working directory you want to use
list.files("C:/Users/ERICK/Desktop/R PROGRAMMING")
# read in the data from the local files. first set the working directory where the
#file are
#setwd("C:/Users/ERICK/Desktop/R PROGRAMMING")

# read the files into R
setwd("C:/Users/ERICK/Desktop/R PROGRAMMING/test")
Test_data_x <- read.table("X_test.txt")
Test_data_y <- read.table("y_test.txt")
Subject_text <- read.table("subject_test.txt")
#Then connect the Testdatx and y and the subject
Test <- cbind(Test_data_x,Test_data_y,Subject_text)
#Try to check the structure of the data.
str(Test)
# make another data set for the Training. 
Train_data_x <- read.table("X_train.txt")
Train_data_y <- read.table("y_train.txt")
Subject_train <- read.table("subject_train.txt")
Activity_labels <- read.table("activity_labels.txt")
#load also the features for the names so that we'll rename the columns later using this
features  <- read.table("features.txt")
#Combine the Train datax, train data y and subject train data into one data set.
#called train
Train <- cbind(Train_data_x,Train_data_y,Subject_train)

# you need to merge the Test and Train into one data.
Data1 <- rbind(Test,Train)
#Load the dplyr package to help you analyse the dataset
library(dplyr)
library(tidyr)
# There re two coluns that are same V1  and V1. we ned to unite them into one variable
#unite(Data1, V1:v1, remove = FALSE)
# we can use the name_repair from tibble library tgo correct this error of dublicate names
#remove the duplicated names of the columns v1 and v1
Data2 <- Data1[, !duplicated(colnames(Data1))]
# you will remain with the only needed columns
# You can now search for the mean() and std() by using filter
library(dplyr)
str(Data2)
# when you consider the features[ names of the columns]we need to organize and search for totall mean()
str(features)
tolower(names(features))
features_selected <- select(features, V2)# so that we can only have the names of the features
# change the features to string
#table(features2$V2 == "tBodyAcc-mean()-X")
#strsplit(V2(features2, "-."))
#change features 3 to a factor variable so that you can use the strsplit to split the names
features_selected <- features2[1:561, ]
str(features_selected)
features3 <- as.vector(unlist(features_selected$V2))
tolower(features3)
# Make all the characters be stanadrd i.e to lower
means_summary <- gsub("-", "", (features3))
table(grepl("mean", means_summary))
std_summary <- gsub("-", "", (features3))
table(grepl("std", means_summary))
# the above will give you the summary of std() and mean()
# for the 3rd section we need to add the activities labels
arrange(std_summary,)
Activities_index1 <- as.vector(unlist(Activity_labels$V2))
Activities_index2 <- c("WALKING"= 1, "WALKING_UPSTAIRS"= 2, "WALKING_DOWNSTAIRS"= 3, "SITTING"= 4, "STANDING"= 5, "LAYING"= 6)
tolower(std_summary)
# labeled dataset to make one data frame, you need to set names
#to replace the column names  the data and the variable names
std_summary2 <- substring(std_summary, first = 2,   last = 30)
Labed_dataset <- as.vector(setNames(1:561, std_summary2))
#You need to have descriptive variable names. therefore, i have replaced t, with time in the variable
#and f with frq for frequency.the std..is just activities names
#create an independent tidy data set from the above.
library(tidyverse)
library(dplyr)

#Make one dataset now with labed activity names
#combine the data and the Activities to make one dataset for final 
#analysis
# I want to have 2 lists from the [data2 and descriptive labes(labed dataset)
Df1 <- as.list(Data2)
Df2 <- as.list(Labed_dataset)
Df4 <- sapply(Df1, mean)
Df3 <- cbind(Df1, std_summary2)
# lets try to come up with one properly named dataframe
as_data_frames_activities <- as.data.frame(std_summary2)
as_data_frames_values <- as.data.frame(Data2)
library(tibble)
#The following data frame has the rows now changed to columns. now you can merge 
#the two frames to come up with one dataset. use the transpose function t()
Data_frame_1 <- as_data_frames_values
Data_frame_2 <- as.data.frame(t(as_data_frames_activities))
#combine the two df
#Combined_Data_frames <- merge(Data_frame_1, Data_frame_2, by = c("V1"), all = FALSE)
#lets change the to factors 
Data_frame_1_1 <- as.vector(Data_frame_1)
#Data_frame_1_2 <- as.vector(Data_frame_2)
Data_frame_1_2 <- t(Data_frame_2)
#You need to have a dataframe with the descriptive names now
names(Data_frame_1_1) <- Data_frame_1_2
dd1 <- colMeans(Data_frame_1_1)
#group the data and get the clean data with summary of mean and meadian
library(tibble)
dd1 <- group_by(Data_frame_1_1)

#You can now group your data  and have the summary of average mean and median
dd2 <- sapply(dd1, function(x) c(  
                         "Mean"= mean(x,na.rm=TRUE),
                         "Median" = median(x)
)
)
dd3 <- as.tibble(dd2)
group_by(dd3)
print(dd3, tibble.width = Inf)
view(dd3)

# make the code book using the Datamaid package

#library(dataMaid)
#makeCodebook(dd3)

# You can now have your clean data in tibble

tidydata <- print(dd3, width = 10000, n = 100000)
write.table(tidydata, "tidydata.txt", row.names = FALSE)

view(tidydata)
