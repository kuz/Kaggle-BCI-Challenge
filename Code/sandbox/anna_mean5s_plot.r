# load libraries
library(caret)
library(data.table)
library(randomForest)
library(reshape2)
library(ggplot2)
library(reshape2)
setwd('/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Code/sandbox/')
# load data
train.path <- '../../Data/raw/train'
train.files <- dir(train.path, pattern='Data_.*\\.csv', full.names=T)
train.subjs <- as.numeric(substr(train.files, 28, 29))
train.sess <- as.numeric(substr(train.files, 35, 36))

# Labels from TrainLabels file for this session
feedback_labels <- fread('../../Data/TrainLabels.csv', sep = ',') #number of feedbacks for first subject

iter <- 6
for(subject in unique(train.subjs)[6:16]){  
  print(iter)
  
  if(subject<10){
    ptrn <- paste('Data_S0',subject,".*\\.csv",sep='')
  } else {
    ptrn <- paste('Data_S',subject,".*\\.csv",sep='')  
  }  
  
  train.files <- dir(train.path, pattern=ptrn, full.names=T)
  train.tables <- lapply(train.files, fread)
  train.nrows <- sapply(train.tables, nrow)
  
  for (i in 1:length(train.tables)) {
    train.tables[[i]] = cbind.data.frame(train.tables[[i]], 'Session'=rep(train.sess[[i]]),
                                         'Subject'=rep(train.subjs[i], train.nrows[i]))
  }
  train.orig <- do.call(rbind, train.tables)

  # Indexes of feedback event
  feedback_labels_subject_seq <- seq(from=1, to=5440, by=339)[iter:(iter+1)]
  print(feedback_labels_subject_seq[1])
  feedback_labels_subject <- feedback_labels[feedback_labels_subject_seq[1]:feedback_labels_subject_seq[2]]
  feedback_strt_indx <- which(train.orig$FeedBackEvent == 1)

  positive_feedback_indexes <- feedback_strt_indx[which(feedback_labels_subject$Prediction == 1)]
  negative_feedback_indexes <- feedback_strt_indx[which(feedback_labels_subject$Prediction == 0)]
  
  # List of stimulus triggering data.frames for each feedback event in the session
  positive_window_list <- lapply(positive_feedback_indexes, function(x) as.data.frame(train.orig)[(x - 1000):(x + 1000),2:(ncol(train.orig)-2)]) #1000/200=5 sec
  negative_window_list <- lapply(negative_feedback_indexes, function(x) as.data.frame(train.orig)[(x - 1000):(x + 1000),2:(ncol(train.orig)-2)])
  
  positive_window_average <- Reduce("+", positive_window_list) / length(positive_window_list)
  negative_window_average <- Reduce("+", negative_window_list) / length(negative_window_list)
  
  # Melting  window average
  melt_positive_average <- melt(positive_window_average)
  melt_negative_average <- melt(negative_window_average)
  
  # Adding time for melted positive data set 
  melt_positive_average$Time <- rep(as.data.frame(train.orig)[1:2001, 1], (length(train.orig[1,])-3))
  melt_positive_average$Feedback_type <- as.factor(rep(1, nrow(melt_positive_average)))

  # Adding time for melted negative data set 
  melt_negative_average$Time <- rep(as.data.frame(train.orig)[1:2001, 1], (length(train.orig[1,])-3))
  melt_negative_average$Feedback_type <- as.factor(rep(0, nrow(melt_positive_average)))
  
  melt_average <- rbind(melt_positive_average, melt_negative_average)
  melt_average_wo_FBE <- subset(melt_average, variable!='FeedBackEvent')
  
  file_name <- paste("../../Figures/stimulus_trigger_5s_average_S0", subject, "_per_feedback_type.pdf",sep='')
  print(file_name)
  p <-  ggplot(melt_average_wo_FBE, aes(x = Time, y = value, group = Feedback_type, colour = Feedback_type)) + 
    geom_line() + theme_bw()+
    facet_wrap(~variable)
  pdf(file=file_name, height = 12, width = 16)
  print(p)  
  dev.off()
 iter <- iter+1
}