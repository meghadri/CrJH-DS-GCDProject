## Merging
##
##  (paths are relative to the "UCI HAR Dataset" folder into which the downloaded file is unzipped by default)
##
##  feature.txt
##      This contains the 'features' i.e. column names of the data sets in the 
##  y_test.txt 
##      This contains the Activity Labels i.e. row names corresponding to each of the rows in the signals sets
##  subject_train.txt 
##      This contains the subject participants in the study and each line corresponds to a Window sample
##  The approach used may not be the most efficient in terms of memory usage
##
##  X_[test|train].txt and y_[test|train].txt contain the actual datasets
##      Inertial signals data is not considered in this submission
##
##  The assignment requires the 'readable' names. Since I am not familiar with the domain, the original measurement names
##      are used as the column names for the mean values of the readings
##  Measurements whose labels contained -mean(), -std(), -meanFreq() were the ones used. This based on the 
##      requirement that Means and Standard Dev measures should be the ones used. 
##
##  The final data frame created in the last statement of the script has the subject id in the 1st column and the activity 
##      name in the 2nd column followed by the 79 measurements of interest. Each cell in a column contains the average (mean)
##      of the samples of that measurement provided in the data files by subject id and activity.
##
##

## BEGIN: Utility functions to help download and unpack the files
Fn_fetchRemoteFileAndExpand <- function(remoteFileURL, destDir, localFName=NULL, unzip=TRUE) {
    require(tools)
    
    if(!file.exists(destDir)) 
        dir.create(destDir)
    
    if(is.null(localFName) || is.na(localFName) || nchar(localFName) == 0 ) {
        destFile <- file_path_sans_ext(basename(URLdecode(remoteFileURL)))
    } else {
        destFile = localFName
    }
    
    fileExt <- file_ext(basename(URLdecode(remoteFileURL)))
    destFile <- paste(destDir, .Platform$file.sep, destFile, "_", as.Date(Sys.time()), ".", fileExt, sep="")
    
    download.file(remoteFileURL, destFile, method = "curl")
    
    if(unzip == TRUE)
        dataFile <- unzip(destFile, exdir=destDir)
    
    dataFile
}


GCD_Proj1_LoadData <- function() {
    ## Download the data file into a sub-folder 'data' of the current working directory
    ##  It will be created is not present.
    ##  The local file name will be identical to that of the remote zip file, with the addition of 
    ##  of the download date appended to the file name (& before the . file extension).
    ##  Fn_fetchRemoteFileAndExpand returns the unzipped file - qualified with its path relative to the
    ##  working directory
    remoteFileURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    destLocalDir <- "data"
    dataFiles <- Fn_fetchRemoteFileAndExpand(remoteFileURL, destLocalDir)
    
    dataFiles
}

## END - Util functions

GCD_Proj1_LoadData()

## top leve base folder for the data
folderBase <- "data/UCI HAR Dataset/"

## Read the data sets common to both test and training data
df_ActivityLabels <- read.table(paste(folderBase, "activity_labels.txt", sep=""), header = FALSE, numerals=c("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE)

df_Features <- read.table(paste(folderBase, "features.txt", sep=""), header = FALSE, numerals=c("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE)


## We are only looking for features (measurements) of the mean and stddev
##  Examination of the features.txt file shows that the names for these 
##  consistently contain "-mean()", "-std()", or "-stdFreq()". 
##  Hence we extract the entries that contain these from df_Features, along
##  with the column index

## vector with the ordinals of the features of interest
idxMean <- grep("-mean\\(\\)", df_Features$V2)
idxStd <- grep("-std\\(\\)", df_Features$V2)
idxMeanFreq <- grep("-meanFreq\\(\\)", df_Features$V2)

## We know that all the cols contain doubles (numeric)
## Construct a colClasses vector to be used in the call to read.table
##  to extract only the cols of interest from the file

colCls <- c(rep(NA), 561)
colNms <- character()

## Could be better than one of the apply functions, but need to get more comfortable with them!
## Get the measurement (column names)
for(i in seq(1,561,1)) {
    
    colCls[i] <- "NULL"
    if((i %in% idxMean) | (i %in% idxStd) | (i %in% idxMeanFreq)) {
        colCls[i] <- "numeric"
        colNms <- append(colNms, df_Features$V2[i])
    }
}

##
##  This load the test and training data. Need to supply either "test/" or "train/" as the parameter. The code assumes the path variables set in earlier lines
## 
loadSetTypeData <- function(folder="test/", xfile="X_test.txt", yfile="y_test.txt", subjfile="subject_test.txt") {
    ## Test data folder
    folderForSet <- paste(folderBase,folder, sep="")
    
    fileX <- xfile
    fileY <- yfile
    fileSubj <- subjfile
    
    ## Read in the columns which hold the measurements (features) of interest
    dfDataSet <- read.table(paste(folderForSet, fileX, sep=""), header = FALSE, numerals=("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE, colClasses = colCls)
    
    ## Add activity labels to the dataset
    df_Y <- read.table(paste(folderForSet, fileY, sep=""), header = FALSE, numerals=c("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE)
    colLabels <- apply(df_Y, c(2), function(x) { df_ActivityLabels$V2[x] })
    
    ## Add the subject info to the dataset (as the last column)
    df_Subj <- read.table(paste(folderForSet, fileSubj, sep=""), header = FALSE, numerals=c("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE)
    dfDataSet <- cbind(colLabels, dfDataSet)
    dfDataSet <- cbind(df_Subj$V1, dfDataSet)
    
    ##    dfDataSet <- cbind(colLabels, dfDataSet)
    
    ## Add the subject info to the dataset (as the last column)
    ##    df_Subj <- read.table(paste(folderForSet, fileSubj, sep=""), header = FALSE, numerals=c("no.loss"), blank.lines.skip = TRUE, stringsAsFactors = FALSE)
    ##    dfDataSet <- cbind(dfDataSet, df_Subj$V1)
    
    ## All the column names, except for the newly added one has been named (for now). Setting name of the new column to 'activity' viz. represents one of the Activity Labels
    colnames(dfDataSet) <- c(c('subject_id', 'activity'), colNms)
    ##    colnames(dfDataSet) <- c(c('activity'), colNms, c('subject_id'))
    
    dfDataSet[,"activity"] <- as.factor(dfDataSet[,"activity"])
    dfDataSet[,"subject_id"] <- as.factor(dfDataSet[,"subject_id"])
    
    dfDataSet
}

dfDataSet_Test <- loadSetTypeData(folder="test/", xfile="X_test.txt", yfile="y_test.txt", subjfile="subject_test.txt")
dfDataSet_Train <- loadSetTypeData(folder="train/", xfile="X_train.txt", yfile="y_train.txt", subjfile="subject_train.txt")

if((ncol(dfDataSet_Test) != ncol(dfDataSet_Train)) || any(colnames(dfDataSet_Test) != colnames(dfDataSet_Train)) == TRUE)
    stop("Unexpected error occurred - A mismatch was found between the columns of the extracted Test and Training data sets.")

## 
## Merge the tables 
##  Appending a new column to the tables to indicate whether it was test or training data.
##
numCols <- ncol(dfDataSet_Test)

newCol <- c(rep("training", nrow(dfDataSet_Train)))
dfDataSet_Train <- cbind(dfDataSet_Train, newCol)
colnames(dfDataSet_Train)[numCols+1] <- "source"
dfDataSet_Train[,"source"] <- as.factor(dfDataSet_Train[,"source"])

newCol <- c(rep("test", nrow(dfDataSet_Test)))
dfDataSet_Test <- cbind(dfDataSet_Test, newCol)
colnames(dfDataSet_Test)[numCols+1] <- "source"
dfDataSet_Test[,"source"] <- as.factor(dfDataSet_Test[,"source"])

dfMerged <- merge(dfDataSet_Test, dfDataSet_Train, all=TRUE)

if(nrow(dfMerged) != (nrow(dfDataSet_Test) + nrow(dfDataSet_Train))) 
    stop("Unexpected error occurred - The number of rows in the merged data set was different than expected.")

generateMeans <- function(dfMerged=dfMerged) {
    colNames <- colnames(dfMerged)
    colNames <- colNames[seq(1,length(colNames)-1,1)]
    
    ##        dfMeans <- data.frame(subject=numeric(), activity=character(), measurement=character(), average=numeric())
    
    dataRes <- data.frame(t(rep(NA, length(colNames))) )
    colnames(dataRes) <- colNames
    ## remove NAs
    dataRes <- dataRes[-1,]
    
    for(subj in levels(dfMerged$subject_id)) {
        df1 <- dfMerged[(dfMerged$subject_id==subj),]
        lstActivity <- list()
        
        for(act in levels(df1$activity)) {
            ds <- dfMerged[((dfMerged$subject_id==subj) & (dfMerged$activity==act)), ]
            ## 1st 2 cols are subject_id & activity, the last is the source of the data
            cols <- colnames(ds)
            cols <- cols[seq(3,ncol(ds)-1,1)]
            lstMeans <- list()
            
            for(c in cols) {
                mean <- colMeans(ds[c])
                lstMeans[c] <- mean 
            }
            
            vecMeans <- sapply(lstMeans, paste0, collapse=",")
            dfSubjAct <- as.data.frame(t(c(subject_id=subj, activity=act)))
            dfVecMeans <- as.data.frame(t(vecMeans))
            dfTmp <- merge(dfSubjAct, dfVecMeans, all=TRUE)
            dataRes <- merge(dataRes, dfTmp, all=TRUE)
        }
        
    }
    
    dataRes    
}

dfMeans <- generateMeans(dfMerged)
##print(paste("Sub iters =", iterSubj, "Act iters =", iterAct, "Means iters = ", iterMeans))



