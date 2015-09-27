library(dplyr)

DATA_DIR <- "./UCIHARDataset"


# Reads files from disk.
LoadData <- function() {
  if(!file.exists(DATA_DIR)) {
    print("DATA_DIR do not exist. Please provide the dataset dir.")
    stop()
  }
  
  # X_train data
  XL <- read.table(paste(DATA_DIR, 'train/X_train.txt', sep='/'))
  # Subject Train Data
  SL <- read.table(paste(DATA_DIR, 'train/subject_train.txt', sep='/'))
  # activity id Data
  YL <- read.table(paste(DATA_DIR, 'train/y_train.txt', sep='/'))
  
  # X_test data
  XT <- read.table(paste(DATA_DIR, 'test/X_test.txt', sep='/'))
  # Subject Test Data
  ST <- read.table(paste(DATA_DIR, 'test/subject_test.txt', sep='/'))
  # activity id Data
  YT <- read.table(paste(DATA_DIR, 'test/y_test.txt', sep='/'))
  
  return(list(x.train=XL, subject.train=SL, y.train=YL,
              x.test=XT, subject.test=ST, y.test=YT))
}


# This glues together the train rows (7352) with the test rows (2947)
# and also the subject and activites columns to the right.
# This gives a data set with the following dimensions:
# > dim(ClipData(LoadData()))
# [1] 10299   563
ClipData <- function(raw.data = NULL) {
  x.train <- raw.data$x.train
  subject.train <- raw.data$subject.train
  y.train <- raw.data$y.train
  
  x.test <- raw.data$x.test
  subject.test <- raw.data$subject.test
  y.test <- raw.data$y.test
  
  # Puts a descriptive column/variable name to subjects and activities.
  names(subject.train) <- c("SubjectId")
  names(subject.test) <- c("SubjectId")
  names(y.train) <- c("ActivityLabelId")
  names(y.test) <- c("ActivityLabelId")
  
  # Appends to the right the column corresponding to subjects.
  full.train <- cbind(x.train, subject.train)
  full.test <- cbind(x.test, subject.test)
  
  # Glue the test data set to the bottom of the train data set.
  dataset.baseline <- rbind(full.train, full.test)
  
  # Glue the test activites list to the bottom of the train activities list.
  y.data <- rbind(y.train, y.test)
  
  # Appends to the right the column corresponding to the activities.
  dataset.baseline <- cbind(dataset.baseline, y.data)
  
  return(dataset.baseline)
}


# This function seeks which indexes are a given measure inside the file by string pattern,
# this will correspond to determined columns in the full glued dataset baseline.
FeaturesIndex <- function(feature.pattern = '', file.path = '') {
  if(!file.exists(file.path)) {
    print("Please provide an existing feature files path.")
    stop()
  }
  
  file.data <- read.table(file.path)
  measure.index <- grep(feature.pattern, file.data$V2, ignore.case=FALSE, value=FALSE)
  
  return(measure.index)
}


# From the full dataset baseline extracts only the relevant columns (mean and std).
# > dim(ExtractMeanAndStd(ClipData(LoadData())))
# [1] 10299    81
ExtractMeanAndStd <- function(input.data = NULL) {
  # Grabs the indexes (columns) for the dataset baseline for mean and std.
  mean.index <- FeaturesIndex('mean', paste(DATA_DIR, 'features.txt', sep='/'))
  std.index <- FeaturesIndex('std', paste(DATA_DIR, 'features.txt', sep='/'))
  
  # Subset the dataset baseline based on the grabbed interest indexes (columns).
  mean.data <- input.data[, mean.index]
  std.data <- input.data[, std.index]
  
  # Activity and Subject data is added with the purpose of keep consistency
  # through the whole process even though it's required only mean and std data.
  activities.data <- input.data$ActivityLabelId
  subjects.data <- input.data$SubjectId
  
  extracted.data <- cbind(mean.data, std.data, ActivityLabelId=activities.data, SubjectId=subjects.data)
}


# Reads the activity labels data giving meaningful labels (columns names).
ActivityLabels <- function(file.path='') {
  if(!file.exists(file.path)) {
    print("Please provide an existing file path for activity labels.")
    stop()
  }
  
  activity.labels <- read.table(file.path)
  names(activity.labels) <- c('ActivityLabelId', 'ActivityLabel')
  
  return(activity.labels)
}


# Merges the relevant extracted mean and std data with the activities data info.
MergeData <- function(input.data = NULL, activity.data = NULL) {
  merge.data <- merge(input.data, activity.data, by.x="ActivityLabelId", by.y="ActivityLabelId", sort=FALSE)
  
  return(merge.data)
}


# Adds meaningful understandable readable human column names to the
# relevant extracted data set containing mean and std info.
AddSelfExplainNames <- function(dataset, featurelist.path) {
  dataset.names <- names(dataset)
  
  if(!file.exists(featurelist.path)) {
    print("Please provide an existing file path for features list.")
    stop()
  }
  
  features <- read.table(featurelist.path)
  raw.colnames <- dataset.names[!dataset.names %in% c("ActivityLabelId", 'ActivityLabel', 'SubjectId')]
  target.names <- gsub("V", "", raw.colnames)
  
  raw.feat.names <- features[as.numeric(target.names), ]
  raw.feat.names$V2 <- sapply(raw.feat.names$V2, FUN=GenerateHumanReadableName)
  
  for(x in raw.feat.names$V1) {
    position <- match(paste("V", x, sep=''), dataset.names)
    new.name <- raw.feat.names[raw.feat.names$V1 == x,]$V2
    #num.row <- as.numeric(sub("V", '', x))
    
    names(dataset)[position] <- new.name
  }
  
  #Arrange in favor of readability
  dataset <- arrange(dataset, SubjectId, ActivityLabelId)
  
  return(dataset)
}


# Translates features cryptic labels to more human understandable column names.
GenerateHumanReadableName <- function(not.readable = NULL) {
  tokens <- list(
    "Body" = "Body",
    "Acc" = "Acceleration",
    "Gyro" = "Gyroscope",
    "Jerk" = "Jerk",
    "mean()" = "Mean",
    "std()" = "StandardDeviation",
    "meanFreq()" = "MeanFreq",
    "Mag" = "Magnitude",
    "-" = '',
    "-X" = "AtXAxis",
    "-Y" = "AtYAxis",
    "-Z" = "AtZAxis",
    "BodyBody" = "Body",
    "FrequencyFrequency" = "Frequency"
  )
  
  measure.key <- substr(not.readable, 0,1)
  not.readable <- toString(not.readable)
  
  if( measure.key %in% c('t', 'f')) {
    not.readable <- substr(not.readable, 2, nchar(not.readable))
    
    if (measure.key == 't'){
      not.readable <- paste(not.readable, "Time", sep='')
    }
    
    if (measure.key == 'f') {
      not.readable <- paste(not.readable, "Frequency", sep='')
    }
  }
  
  for(token in names(tokens)) {
    not.readable <- sub(token, tokens[token], not.readable, fixed=TRUE, ignore.case=FALSE)
  }
  
  return(not.readable)
}


# Extracts the summary average data.
CreateSummarized <- function(input.data = NULL) {
  group.input.data <- group_by(input.data, SubjectId, ActivityLabelId, ActivityLabel)
  summ.data <- summarise_each(group.input.data, funs(mean))
  
  return(summ.data)
}


# Script entry point.
GetDataProject.run <- function() {
  
  # Load files.
  raw.data <- LoadData()
  
  # Builds the whole big data set.
  dataset.baseline <- ClipData(raw.data)
  
  # Extracts relevant interest info (columns).
  extracted.data <- ExtractMeanAndStd(dataset.baseline)
  
  # Loads the activities data info.
  activity.data <- ActivityLabels(paste(DATA_DIR, 'activity_labels.txt', sep='/'))
  
  # Merges the extracted data with the activities data.
  merged.data <- MergeData(input.data=extracted.data, activity.data=activity.data)
  
  # Gives meaningful column names to the data set.
  human.readable.dataset <- AddSelfExplainNames(merged.data, paste(DATA_DIR, 'features.txt', sep='/'))
  
  # Calculates the averages.
  summ.data <- CreateSummarized(human.readable.dataset)
  
  return(summ.data)
}


# Visualize the tidy data requested.
View(GetDataProject.run())
