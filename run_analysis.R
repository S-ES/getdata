# 1.Merges the training and the test sets to create one data set.

library("plyr")

## Training set

X_train <- read.table("~/getdata.quiz/UCI HAR Dataset/train/X_train.txt", 
                      quote="\")
subject_train <- read.table("~/getdata.quiz/UCI HAR Dataset/train/subject_train.txt", 
                            quote="\")
y_train <- read.table("~/getdata.quiz/UCI HAR Dataset/train/y_train.txt", 
                      quote="\")
colnames(subject_train)=gsub("V1",
                             "subject",
                             colnames(subject_train))
colnames(y_train)=gsub("V1",
                       "activity.numbers",
                       colnames(y_train))
summary(y_train)
subjects.labels_train=cbind(subject_train,
                            y_train)
str(subjects.labels_train); summary(subjects.labels_train)
features <- read.table("~/getdata.quiz/UCI HAR Dataset/features.txt", 
                       quote="\")
features.names<-features[,2]
x<-X_train

for (i in 1:length(features.names))
{
    colnames(x)=gsub(colnames(x)[i],
                     features.names[i],
                     colnames(x))
}
    
complete.X_train=cbind(subjects.labels_train,x)

## for Test
list.files(path = "~/getdata.quiz/UCI HAR Dataset/test")

X_test <- read.table("~/getdata.quiz/UCI HAR Dataset/test/X_test.txt", 
                     quote="\")
subject_test <- read.table("~/getdata.quiz/UCI HAR Dataset/test/subject_test.txt", 
                            quote="\")
y_test <- read.table("~/getdata.quiz/UCI HAR Dataset/test/y_test.txt", 
                      quote="\")

colnames(subject_test)=gsub("V1",
                             "subject",
                             colnames(subject_test))
str(subject_test)
str(X_test)
colnames(y_test)=gsub("V1",
                       "activity.numbers",
                       colnames(y_test))
summary(y_test)
subjects.labels_test=cbind(subject_test,
                           y_test)
str(subjects.labels_test); summary(subjects.labels_test)

x.test<-X_test

for (i in 1:length(features.names))
{
    colnames(x.test)=gsub(colnames(x.test)[i],
                     features.names[i],
                     colnames(x.test))
}

write.table(x.test,
            file="complete.X_test.txt",
sep="\t")

# Add a status line to differ tests for train
if status==1 => train
if status==2 => test
library(plyr)
complete.X_train.1<-mutate(.data = complete.X_train,
                           status=1)
complete.X_test.1<-mutate(.data = complete.X_train,
                          status=2)

data.base=merge(x = complete.X_train.1,
                y = complete.X_test.1,
                all=T)
dim(data.base)

colnames(data.base)<-sub("activity.labels",
                         "activity.numbers",
                         colnames(data.base))

write.table(x=data.base,
            file="data.base.txt",
            sep="\t")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement

data.base.mean.stf=data.base[grep("mean|std",
     colnames(data.base),
     ignore.case = T),]


# 3. Uses descriptive activity names to name the activities in the data set

activity.labels<-read.table("~/getdata.quiz/UCI HAR Dataset/activity_labels.txt",
                            sep=",
                            header=F)

colnames(activity.labels)<-sub("V2",
                                "activity.labels",
                                colnames(activity.labels))

data.base.1=merge(x=data.base,
      y=activity.labels, 
      by.x ="activity.numbers",
      by.y ="V1",
      all=T)

# 4. Appropriately labels the data set with descriptive variable names. 

features=read.delim(file="~/getdata.quiz/UCI HAR Dataset/features.txt",
                    header=F, 
                    sep="-")
features$V1=gsub("[0-9]",",features$V1)
features$V1=gsub(" t","time of ",features$V1)
features$V1=gsub(" f","frequency of ",features$V1)
features$V2=str_replace_all(features$V2, "[[:punct:]]", ")
features$V2=gsub("min","minimum of ",features$V2)
features$V2=gsub("mean","mean of ",features$V2)
features$V2=gsub("std","standard deviation of",features$V2)
features$V2=gsub("max","maximum of",features$V2)
features$V2=gsub("arCoeff","autorregresion coefficient of",features$V2)
features$V2=gsub("mad","median of",features$V2)
features$V2=gsub("sma","signal magnitude area of",features$V2)
features$V2=gsub("iqr","Interquartile range of",features$V2)
features$V2=gsub("bandsEnergy"," Energy of a frequency interval of",features$V2)
features$V2=gsub("maxInds"," index of the frequency of",features$V2)
features.1=cbind(features$V2,features[,c("V1","V3")])
features$V1=gsub("[Freq]",",features$V1)
features.names=paste(features$V2,features$V1, features$V3,sep=" ")
names(data.base.1)=features.names

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sp=split(data.base.1,
      list(data.base.1$subject,
                       data.base.1.$activity.labels))

tidy.data=sapply(sp,
                 function(x) colMeans(x[,3:563]))
write.table(tidy.data,
            file="tidy.data.txt",
            sep="\t")