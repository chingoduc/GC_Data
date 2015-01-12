---
title: "Get&Clean Data "
author: "Chi Ngo"
date: "Sunday, January 11, 2015"
output: html_document
---

### Get Data
We will first get the data contained in the data set provided by UCI, in which:
- X_test and X_train contains measurements/features provided by sensors and
- Y_test and y_train contains activity labels for example activilty number 5
```x_test <- read.table("X_test.txt", header=FALSE)```
```y_test <- read.table("y_test.txt", header=FALSE)```
```x_train <- read.table("X_train.txt", header=FALSE)```
```y_train <- read.table("y_train.txt", header=FALSE)```

### Data Tables Merging
We will combine using rbind the train set and test set of each of x and y data set to get two new data tables: x_fullset and y_fulllset:
```x_fullset <- rbind(x_train, x_test)
y_fullset <- rbind(y_train, y_test)
```
We will get now the list of 561 feature names into a new data table "features". 
For example: feature# 1 is tBodyAcc-mean X which is the mean of
acceleration of of the body in the X direction.
Features # 2 is tBodyAcc-mean Y is similar to feature #1 but in the Y direction.
```features <- read.table("../features.txt")```

### Restriction of the data set to Mean and Std
We will now determine which are the columns the name of which includes either "mean" or "std" because we will only keep thoses features. 
```j <- 1```
```sel <- c()
coln <- c()```
```for (i in 1:nrow(features)) {```
```  if (grepl("mean|std", as.character(features[i,2]))) {
    sel[j] = i 
    coln[j] <- as.character(features[i,2])
    j<- j+1
  }
}
```
Only 79 features are found to with either "mean" or "std" is part of its name."sel" contains index of all the columns with "mean" or"std" and coln contains the corresponding feature names.

```x_fullset <- select(x_fullset,sel)```

names of the 79 features will be added as the column names of the tables of all the measurements.

we delete "(" and ")" in the column names

```coln <- sub("\\(\\)","",coln)
colnames (x_fullset) <- coln```


### Merge y_train and y_test
y_train and y_test contains activity labels, not activity names
```y_train <- read.table("y_train.txt") 
y_test <- read.table("../test/y_test.txt")```       

### New data table: act_fullset 
that contains the list of all activity labels of all the activities carried out by all subjects.
resulting from "rbind" y_train and y_test
```act_fullset <- rbind(y_train, y_test)
act_labels <- read.table("activity_labels.txt")
```
act_labels denotes the activity name for each activity label.
For example, actvity label 2 is "WALKING_UPSTAIRS"
activity label 6 will correspond to "STANDING"

### Replacing activity label by activity name.
All activity label in the act_fullset will be replaced by its name. 
Note that act_fullset contains the list of all activities that had been carried out
```act_fullset[,1] <- as.character(act_labels[act_fullset[,1],2])
colnames(act_fullset) <- "Activity"
```

### Merge Subjects from train set and test set
After the merging, the data table "subj_full" contains the list of subjects that have performed a conresponding activity in the data table "act_fullset"
For example: subj[1050,] = 6 and act_fullset[1050,] = LAYING. This means the subject no 6 has been "laying"

```subj_train <- read.table("./train/subject_train.txt")```
```subj_test <- read.table("./test/subject_test.txt")```
```subj_full <- rbind(subj_train, subj_test)```
```colnames(subj_full) <- "SubjectId"```

### Merge "activity"" then "subject"" and finally "features""
After the cbind of "subj_full" and "x_fullset", "sub_fullset" will contain the list of all measurements
of all subjects. 
For example, row 1 indicates only all the measurements for the subject #1.
```subj_fullset <- cbind(subj_full, x_fullset)```

After the cbind of "act_fullset" and "subj_fullset", the "complete_set" will contain 
the list of all measurements of all activities of all subjects. 
For example, row 1 of the complete_set indicates only all the measurements for the subject #1 
for one of his several STANDING activities. 
```complete_set <- cbind(act_fullset, subj_fullset)```

### Create a data.table from the "complete.set" in order to re-arrange the rows
```cmpl_dt <- data.table(complete_set)```

We will arrange to group all the Activities by each them and then 
group by Each Suject who performs that activity
```act_subj_dt <- arrange(cmpl_dt, Activity, SubjectId)```

### Computing means of measurements per activity and per subject
for example, activity = Laying and by subject no 1
```act_subj_mean <- ddply(act_subj_dt[,], c(.(Activity), .(SubjectId)), colwise(mean))```
```colnames(act_subj_mean)[3:ncol(act_subj_mean)] <- paste(colnames(act_subj_mean)[3:ncol(act_subj_mean)], "_MEAN", sep="")```
```write.csv(act_subj_mean, "act_subj_mean.csv")```

### Calculating standard deviations of measurements per activity and per subject
For example, activity = Laying and by subject no 1
```act_subj_sd <- ddply(act_subj_dt[,], c(.(Activity), .(SubjectId)), colwise(sd))```
```colnames(act_subj_sd)[3:ncol(act_subj_sd)] <- paste(colnames(act_subj_sd)[3:ncol(act_subj_sd)], "_STD", sep="")```

### Merge means and std of by each activity and each subject
```act_subj_meansd <- join(act_subj_mean,act_subj_sd,by=c("Activity","SubjectId") )
```
The result of this join is that all the columns with MEANS are grouped together and all the columns
with Standard deviations will group together.
For example for the same measurement, says tBodyAcc-mean-X, its means and std will stay too distant one to another to facilitate the analysis


### Grouping mean and std of a feature near to each other
```rearrange <-c(1,2)```
```rix <- 3```
```maxi <- length(colnames(act_subj_meansd))/2+1```
```initj <- 2+(length(colnames(act_subj_meansd))/2)```
```maxj <- length(colnames(act_subj_meansd))```
```for (i in 3:maxi) {```
```  listi <- strsplit( colnames(act_subj_meansd)[i], "_")```
```  starti <- listi[[1]][1]```
```  endi <- listi[[1]][2]```
  
```  for (j in initj:maxj) {```
```    listj <- strsplit( colnames(act_subj_meansd)[j], "_")```
```    startj <- listj[[1]][1]```
```    endj <- listj[[1]][2]```
```    if (starti == startj) {```
```      rearrange[rix]<- i```
```      rearrange[rix+1]<-j```
```      rix<-rix+2```
```      break```
```    }```
```  }```
  
```}```

```final_dataset <- as.data.frame(matrix(nrow = nrow(act_subj_meansd)))```
```final_colnames <- c()```
```for (i in 1:ncol(act_subj_meansd)) {
  final_dataset <- cbind(final_dataset, act_subj_meansd[,rearrange[i]])
  final_colnames[i] <- colnames(act_subj_meansd)[rearrange[i]]
}```

```final_dataset <- subset(final_dataset, TRUE, select = -V1)```
```colnames(final_dataset) <- final_colnames```
```write.csv(final_dataset, "finalreport.csv")```
```write.table(final_dataset, "finalreport.txt", row.names=FALSE)```



