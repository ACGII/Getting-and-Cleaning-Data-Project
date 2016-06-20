#File : run_analysis.R

#The most difficult part of this project was identifying or recognizing the pieces #that where to be binded together. After each table is read I put a short #indication of the reference to the table in the original READ.ME file.

# Libraries
library(dplyr)
library(data.table)
library(tidyr)

filesPath<-"C:/Albert/Coursera/Get and Clean Data/data/Dataset/UCI HAR Dataset"

DSubTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
#READ.ME:  Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. – Training Group

DSubTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))
#READ.ME:  Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. – Test Group

# Read event files
DEvtTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
#  READ.ME:  Training labels.

DEvtTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))
#  READ.ME:  Test labels.


#Read data files.
DTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" ))) 
#  READ.ME:  Training set.(of actual Data)
DTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))
#READ.ME:  Test set. (of Actual Data)


# for both Event and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subjectID" and "EventNum"
allSubD <- rbind(DSubTrain, DSubTest)
setnames(allSubD, "V1", "subjectID")
allEvtD<- rbind(DEvtTrain, DEvtTest)
setnames(allEvtD, "V1", "EventNum")

#combine the DATA training and test files
dataTable <- rbind(DTrain, DTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean-X")
FeaturesD <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(FeaturesD, names(FeaturesD), c("featureNum", "featureName"))
colnames(dataTable) <- FeaturesD$featureName

#column names for activity labels
eventLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(eventLabels, names(eventLabels), c("EventNum","EventName"))

# Merge columns
allSubjEvtD<- cbind(allSubD, allEvtD)
dataTable <- cbind(allSubjEvtD, dataTable)






#Reading "features.txt" and extracting only the mean and standard deviation
FeaturesMeanStdD <- grep("mean\\(\\)|std\\(\\)",FeaturesD$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

FeaturesMeanStdD <- union(c("subjectID","EventNum"), FeaturesMeanStdD)
dataTable<- subset(dataTable,select=FeaturesMeanStdD) 


##enter name of Event into dataTable
dataTable <- merge(eventLabels, dataTable , by="EventNum", all.x=TRUE)
dataTable$EventName <- as.character(dataTable$EventName)

#the aggregate method coupled with the arrange method allows us to rearrange the columns so they begin with and are grouped with subjectID followed by EventName
dataAggr<- aggregate(. ~ subjectID - EventName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subjectID,EventName))



#Enhance Names
names(dataTable)<-gsub("^t", "T", names(dataTable))
names(dataTable)<-gsub("^f", "F", names(dataTable))
names(dataTable)<-gsub("std..", "SD", names(dataTable))
names(dataTable)<-gsub("mean..", "MEAN", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# New Names
head(str(dataTable),6)
## Classes 'tbl_df', 'tbl' and 'data.fr



##write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)

