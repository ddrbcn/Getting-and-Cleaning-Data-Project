run_analysis <- function(){
  
features<-read.table("./UCI HAR Dataset/features.txt")  
features<-features[,2];



trainingData<-read.table("./UCI HAR Dataset/train/X_train.txt", col.names=features);
testData<-read.table("./UCI HAR Dataset/test/X_test.txt", col.names=features);
data<-rbind(trainingData, testData);

toMatch <- c(".*mean\\(\\).*$", ".*std\\(\\).*$")
matches <- unique (grep(paste(toMatch,collapse="|"), features, value=FALSE))
data <- data[,matches]

subject_test_data<-read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("Subject"))
subject_train_data<-read.table("./UCI HAR Dataset/train/subject_train.txt", col.names=c("Subject"))
subject<-rbind(subject_train_data, subject_test_data)


testlabels<-read.table("./UCI HAR Dataset/test/y_test.txt", col.names=c("Label"));
trainlabels<-read.table("./UCI HAR Dataset/train/y_train.txt", col.names=c("Label"));
labels<-rbind(trainlabels, testlabels);

for(i in 1:nrow(labels)){
  
  if(as.character(labels[i,1])=="1"){
    labels[i,1] <- "WALKING";
    
  }else if(as.character(labels[i,1])=="2"){
    labels[i,1] <- "WALKING_UPSTAIRS";
  }else if(as.character(labels[i,1])=="3"){
    labels[i,1] <- "WALKING_DOWNSTAIRS";
    
  }else  if(as.character(labels[i,1])=="4"){
    labels[i,1] <- "SITTING";
    
  }else   if(as.character(labels[i,1])=="5"){
    labels[i,1] <- "STANDING";
  }else if(as.character(labels[i,1])=="6"){
    
    labels[i,1] <- "LAYING";
  } 
}  

data<-cbind(subject, labels, data);

return(data);

}