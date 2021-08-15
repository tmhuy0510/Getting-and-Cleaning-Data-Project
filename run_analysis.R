library(tidyr)
library(dplyr)

# Set current working directory
if(!file.exists("Project Data")) {dir.create("Project Data")}
myDir = "./Project Data"
setwd(myDir)

# Download zip file which is dataset
myUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
myDest = "./myData.zip"
download.file(myUrl, myDest)

# Unzip zip file
unzip(myDest)

# Set new current working directory
myDir = "./UCI HAR Dataset"
setwd(myDir)

### Merge train and test sets

# Read train set of data
myTrainDataX = read.table("./train/X_train.txt")
myTrainDatay = read.table("./train/y_train.txt")
myTrainSub = read.table("./train/subject_train.txt")

# Read test set of data
myTestDataX = read.table("./test/X_test.txt")
myTestDatay = read.table("./test/y_test.txt")
myTestSub = read.table("./test/subject_test.txt")

# Add names of variables to train and test data (Data_X)
myVarNames = read.table("./features.txt")[, 2]
names(myTrainDataX) = myVarNames
names(myTestDataX) = myVarNames

# Add name of label to train and test data (Data_y)
myLableName = "Activity"
names(myTrainDatay) = myLableName
names(myTestDatay) = myLableName

# Add name of subject to train and test data (Sub)
mySubName = "VolunteerID"
names(myTrainSub) = mySubName
names(myTestSub) = mySubName

# Extract variables related to mean() and std()
cond = grep("(mean\\(\\)|std\\(\\))", myVarNames)
myCompactTrainDataX = myTrainDataX[, cond]
myCompactTestDataX = myTestDataX[, cond]

# Merge all data related to train set
myMergedTrainData = cbind(myTrainSub, myCompactTrainDataX, myTrainDatay)
# Merge all data related to test set
myMergedTestData = cbind(myTestSub, myCompactTestDataX, myTestDatay)

# Merge train and test set
myMergedAllData = rbind(myMergedTrainData, myMergedTestData)

# Change labels of Activity from numbers to literal words
myMergedAllData$Activity = as.factor(myMergedAllData$Activity)
myActLable = read.table("./activity_labels.txt")[, 2]
attributes(myMergedAllData$Activity)$levels = myActLable

myMergedAllData$VolunteerID = as.factor(myMergedAllData$VolunteerID)

# myMergedAllData is 1st required table

### Create 2nd required table

# Create new col from VolunteerID and Activity
myVolAct = paste(myMergedAllData$VolunteerID, myMergedAllData$Activity, sep = ".")

# Create new table from by binding newly created column to columns of variable of myMergedAllData
myNewData = cbind(myVolAct, myMergedAllData[, -c(1, ncol(myMergedAllData))])

# Split newly created table based on its 1st column and Calculate mean of its variables
myTable = sapply(split(myNewData[, 2:ncol(myNewData)], myNewData[, 1]), colMeans)

# Convert result of calculation into data frame
myTable = as.data.frame(t(myTable))

# Add column VolAct to myTable
myTBL = cbind(VolAct = row.names(myTable), myTable)

# Remove row names of myTBL
row.names(myTBL) = NULL

# Separate "VolAct" col into "VolunteerID" and "Activity" cols
myTBL = separate(myTBL, VolAct, sep = "\\.", into = c("VolunteerID", "Activity"))

# Convert class of "VolunteerID" column to numeric to use arrange() function later
myTBL$VolunteerID = as.numeric(myTBL$VolunteerID)

# Arrange myTBL according to "VolunteerID" column
myTBL = arrange(myTBL, VolunteerID)

# Write result to txt file
write.table(myTBL, file = "../Project Result.txt", sep = ",", row.names = F, col.names = T)

# txt file contains 2nd required table