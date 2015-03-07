library(dplyr)
library(tidyr)

#setwd("~/dropbox/coursera/UCI HAR Dataset")

activity <- read.table("activity_labels.txt", col.names=c("id", "label"))
features <- read.table("features.txt", col.names=c("id", "label"))

ytest <- read.table("./test/y_test.txt", col.names=c("activity")) 
activities_test <- ytest %>% 
    left_join(activity, by=c("activity"="id")) %>% 
    rename(id=activity)
ytrain <- read.table("./train/y_train.txt", col.names=c("activity")) 
activities_train <- ytrain %>% 
    left_join(activity, by=c("activity"="id")) %>% 
    rename(id=activity)

xtest <- read.table("./test/X_test.txt", col.names=features$label, colClasses=c("numeric"))
xtrain <- read.table("./train/X_train.txt", col.names=features$label, colClasses=c("numeric"))

subjects_test <- read.table("./test/subject_test.txt", col.names=c("subject"))
test561 <- bind_cols(activities_test, subjects_test, xtest) 
test561 <- test561[!duplicated(names(test561))] 
test561 <- select(test561, contains("mean"), contains("std"))
test561 <- bind_cols(activities_test, subjects_test, test561)
test561 <- test561 %>% rename(activity = label) %>% select(-id)

subjects_train <- read.table("./train/subject_train.txt", col.names=c("subject")) 
train561 <- bind_cols(activities_train, subjects_train, xtrain)
train561 <- train561[!duplicated(names(train561))] 
train561 <- select(train561, contains("mean"), contains("std"))
train561 <- bind_cols(activities_train, subjects_train, train561)
train561 <- train561 %>% rename(activity = label) %>% select(-id)

five61 <- bind_rows(test561, train561)

summary_set <- five61 %>% group_by(activity, subject) %>% summarise_each(funs(mean))
write.table(summary_set, "UCI_HAR_Summary.txt", row.name=FALSE)