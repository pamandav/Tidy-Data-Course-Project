##Getting and Cleaning Data - Course Project

print("Check if data directory exists/ create if not")
require(plyr)

if(!file.exists("./data")){
        dir.create("./data")
}
##Download the data set.
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./data/source.zip")
#unzip("./data/source.zip", exdir="./data/source")

print("Downlaoding and Unzipping the data complete")

##Set the training and test directories
dirPath <- "./data/source/UCI HAR Dataset/"
testDir <- paste(dirPath,"test/",sep="")
trainDir <- paste(dirPath,"train/",sep="")

##Set the files needed.
testData <- read.csv(paste(testDir,"X_test.txt",sep=""),sep="", header=FALSE)
trainData <- read.csv(paste(trainDir,"X_train.txt",sep=""),sep="", header=FALSE)
featuresData <- read.csv(paste(dirPath,"features.txt",sep=""),sep="", header=FALSE)
testActivitiesData <- read.csv(paste(testDir,"y_test.txt",sep=""),sep="", header=FALSE)
trainActivitiesData <- read.csv(paste(trainDir,"y_train.txt",sep=""),sep="", header=FALSE)
activityLabelData <-read.csv(paste(dirPath,"activity_labels.txt",sep=""),sep="", header=FALSE)
##Fetch subject data for training and test data sets
testSubjectData <-read.csv(paste(testDir,"subject_test.txt",sep=""),sep="", header=FALSE)
trainSubjectData <-read.csv(paste(trainDir,"subject_train.txt",sep=""),sep="", header=FALSE)

##Get the field names
fieldNames <- as.vector(unlist(featuresData[2]))

##assign field names to the data sets.

names(trainData) <- fieldNames

##Bind activities column as well.
testData <- cbind(testData,testActivitiesData,testSubjectData)
names(testData) <- c(fieldNames,"activity","subject")
##Rename the activity field appropriately


##Do the same for training data set.
trainData <- cbind(trainData,trainActivitiesData,trainSubjectData)
names(trainData) <- c(fieldNames,"activity","subject")

##Filter only the fields which contain mean() or std() in addition to activity
neededColumns <- c(fieldNames[grepl("mean",fieldNames)],fieldNames[grepl("std",fieldNames)],"activity","subject")


##Select only the specified fields.
refinedTestData <- testData[,neededColumns]
refinedTrainData <- trainData[,neededColumns]

##Merge training and test data sets
mergedInput <- merge(refinedTestData,refinedTrainData, all=TRUE)

##Name the fields in activity labels data set
names(activityLabelData)<- c("activityId","activityLabel")

##Join the merged data set with activity labels.
describedInput <- merge(mergedInput,activityLabelData,by.x="activity",by.y="activityId",all=TRUE)

##To fetch meaning variable names. Has anonymous function to fetch
## descriptive variable name
variablenames <- sapply(names(describedInput),function(x){
                                                ans <-x
                                                if(substring(ans,1,1)=="f")
                                                {
                                                        ans <-paste("FFT_",substring(ans,2),sep="")
                                                }
                                                else if(substring(ans,1,1)=="t")
                                                {
                                                        ans <-paste("Time_",substring(ans,2),sep="") 
                                                }
                                                else
                                                {
                                                        ans <- gsub("angle\\(","Angle_",ans)
                                                }
                                                
                                                ans <- gsub("[()]", "", ans)
                                                ans <- gsub("[-,]","_",ans)
                                                ans
                                        })

##Set the meaningful variables names to the data set.
names(describedInput) <- variablenames

##Create independent tidy data set with the average of each variable for each 
##activity and each subject.

numericCols <- !(colnames(describedInput) %in% c("activityLabel"))
                    
describedInput[,numericCols] = apply(describedInput[,numericCols], 2, function(x) as.numeric(as.character(x)))
meltedInput <- melt(describedInput, id.vars=c("subject","activityLabel"))
groupedInput <- ddply(meltedInput,.(subject, activityLabel,variable),summarize,mean = mean(value))
finalOutput <- dcast(groupedInput,subject+activityLabel~variable, value.var="mean")
finalOutput <- finalOutput[,setdiff(names(finalOutput),"activity")]
print("Witing final output")
write.table(finalOutput, file="tidydata.txt",col.names=TRUE,row.name=FALSE)





