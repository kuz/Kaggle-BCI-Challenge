#libraries
library(zoo)
library(caret)
library(data.table)
library(randomForest)
library(reshape2)
library(xtable)
library(ggplot2)


setwd("/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data")
#---Data---#
Data_S02_Sess01 <- fread("train/Data_S02_Sess01.csv")
train <- as.data.frame(Data_S02_Sess01)
train$Prediction <- ifelse(train$FeedBackEvent==1,TrainLabels$Prediction,0)

#cor_table <- print(xtable(cor(train)),'html',"correlation_between_channels.html")

cor_plot <- qplot(x=X1, y=X2, data=melt(cor(train[,-c(1,59,60)], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

#1. find correlations for ech session and each subject separately, find overlapping channels
#2. find correlations within a subject

highly_correlated <- function(x, correlation_threshold){
  data <- fread(x)
  data <- data[,-c(1,59),with=FALSE]
  cor_idx <- findCorrelation(cor(data), cutoff = correlation_threshold)
  correlated_channels <- names(data)[cor_idx]
  return(correlated_channels)
}

filenames <- list.files("train", pattern="Data.*.csv", full.names=TRUE)
subjects <- unique(substr(filenames, start = 12, stop = 14))
cat("Results of correlations within subject",
    file="correlated_channels_per_subject.txt",sep="\n")

  ldf <- lapply(subject_sessions, highly_correlated, correlation_threshold=0.99)
  intersection_of_corr_channels_sessions <- Reduce(intersect, ldf) 
  cat(paste("subject is", subject),file="correlated_channels_per_subject.txt",append=TRUE, sep="\n")
  cat(intersection_of_corr_channels_sessions,file="correlated_channels_per_subject.txt",append=TRUE, sep="\n")
}

pdf("cor_plots.pdf")
for(subject in subjects){
  subject_sessions <- filenames[grep(paste(".Data_",subject,".",sep=''), filenames)]
  dt <- do.call("rbind.data.frame",lapply(subject_sessions, fread))
  cor_plot <- qplot(x=X1, y=X2, data=melt(cor(dt[,-c(1,59),with=F], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
  print(cor_plot)
}
dev.off()

filenames <- list.files("train", pattern="Data.*.csv", full.names=TRUE)
zt <- lapply(filenames, fread)
dt <- do.call("rbind.data.frame",zt)
write.table(dt, "full_raw_data.txt", sep=',', col.names=T,row.names=F)

data <- dt[,-c(1,59),with=FALSE]
cor_idx <- findCorrelation(cor(data), cutoff = 0.9)
correlated_channels <- names(data)[cor_idx]
write.table(correlated_channels, "correlated_channels_on_full_train_90.txt", quote=F,col.names=F,row.names=F)

train_sub <- train[16000:25000,]
train_melted <- melt(train_sub, id.vars=c("Time","FeedBackEvent","Prediction"))

channels <- unique(train_melted$variable)
few_channels <- subset(train_melted, variable%in%channels[4:7])
feedback <- unique(subset(few_channels, FeedBackEvent==1)$Time)
is_positive <- subset(few_channels, FeedBackEvent==1)$Prediction
is_positive <- is_positive[1:length(feedback)]
ggplot(few_channels,aes(x=Time, y=value, color=variable))+geom_line()+
  theme_bw()+geom_vline(aes(xintercept=feedback[c(1,2)],color=as.factor(is_positive)[c(1,2)]))


ts_AF3 <- train[,c(1,5,59,60)]
ts_AF3_ts <- as.ts(ts_AF3[,2],start=1,frequency=12)
hwf <- HoltWinters(ts_AF3_ts)
Data_S06_Sess04 <- fread("train/Data_S06_Sess04.csv")
test <- as.data.frame(Data_S06_Sess04)

TrainLabels <- fread("train/TrainLabels.csv")
TrainLabels <- as.data.frame(TrainLabels)
TrainLabelsSubj <- TrainLabels[grep("S02_Sess01", TrainLabels$IdFeedBack, value=FALSE,fixed=FALSE),]
TestLabelsSubj <- TrainLabels[grep("S06_Sess04", TrainLabels$IdFeedBack, value=FALSE,fixed=FALSE),]

extract <- function(dataset) {
  fb.idx <- which(dataset$FeedBackEvent == 1)
  result <- data.frame()
  for (fbi in fb.idx) {
    Cmeans <- apply(dataset[fbi:(fbi + 3),-c(1,ncol(dataset))],2,mean)
    result <- rbind.data.frame(result, Cmeans)
  }
  return(result)
}
train.orig <- extract(train)
names(train.orig) <- names(train[,-c(1,ncol(train))])
test.orig <- extract(test)
names(test.orig) <- names(test[,-c(1,ncol(test))])

train.orig$label <- TrainLabelsSubj[,2]
test.orig$label <- TestLabelsSubj[,2]

mod1 <- glm(as.factor(label)~.,data=train.orig, family="binomial")
#TrainLabels$FeedBackNr <- as.numeric(str_sub(TrainLabels$IdFeedBack,start=-3))


#train.files <- dir(train.path, pattern='Data.*\\.csv', full.names=T)
#test.files <- dir(test.path, pattern='Data.*\\.csv', full.names=T)

#train.tables <- lapply(train.files, fread)
#test.tables <- lapply(test.files, fread)
#train.orig <- do.call(rbind, train.tables)
#test.orig <- do.call(rbind, test.tables)

# add labels
labels <- fread('../../Data/TrainLabels.csv')
train.orig <- cbind.data.frame(train.orig, labels$Prediction[1:nrow(train.orig)])

# add decent names
train.orig[, ncol(train.orig)] <- as.factor(train.orig[, ncol(train.orig)])
colnames(train.orig) <- c(paste("A_", 1:(length(colnames(train.orig)) - 1), sep=""), 'class')
train.orig$class <- as.factor(ifelse(train.orig$class == 1, "positive", "negative"))

colnames(test.orig) <- c(paste("A_", 1:(length(colnames(test.orig))), sep=""))

# split into train and validation set
train.idx <- sample(nrow(train.orig), nrow(train.orig) * 2/3)
train <- train.orig[ train.idx, ]
valid <- train.orig[-train.idx, ]
test  <- test.orig

# train a model
trcontrol <- trainControl(method='cv', number = 10, classProbs=T, summaryFunction=twoClassSummary)
preproc   <- c('center', 'scale')
classifier <- train(train[,-ncol(train)], as.factor(train[,ncol(train)]), 'gbm', 
                    trControl=trcontrol, preProc=preproc)

# validation results
predicted.prob <- predict(classifier, newdata=valid[, -ncol(valid)], type="prob")$positive
roc(valid$class, predicted.prob)

# make predictions on test data
predicted.prob <- predict(classifier, newdata=test, type="prob")$positive

# store the results
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep=',', header=T))
result$Prediction = predicted.prob
write.table(result, '../../Results/subKUZ_sandbox.csv', sep=',', quote=F, row.names=F, col.names=T)


#---Functions---#
fourier_transform_on_window <-  function(x){
  window <- 1:length(x)
  trend <- lm(x~window)
  detrended_data <- trend$residuals
  fourier_components_norm <- abs(fft(detrended_data))/(length(detrended_data)/2)
  fourier_components_half <-fourier_components_norm[1:(length(fourier_components_norm)/2)]
  return(fourier_components_half)
}
pieces <- function(dt, window_size=200, step=200){
  rollapply(dt, window_size, by=step, function(x) fourier_transform_on_window(x))
}
fft_per_ch_per_slice <- function(dt, window_size=200, step=200){
  fft_per_ch_per_slice <- c()
  for(electrode in 2:(ncol(dt)-2)){
    print(electrode)
    fft_per_slice <- pieces(dt[,electrode],window_size,step)
    fft_per_ch_per_slice <- cbind(fft_per_ch_per_slice,fft_per_slice)
  }
  return(fft_per_ch_per_slice)
}


fft_per_ch_per_slice <- fft_per_ch_per_slice(Data_S02_Sess01, window_size=200, step=200)

file_out <- "/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/fft_per_ch_per_slice.txt" 
write.table(fft_per_ch_per_slice,file_out,
            col.names=FALSE, row.names=FALSE, quote=F, sep=",")


#------------#
idx_feedback_true <- which(Data_S02_Sess01$FeedBackEvent==1)

#dt_feedback_true <- Data_S02_Sess01[idx_feedback_true[1]:(idx_feedback_true[1]+799),]
#fft_per_ch_per_slice_ev <- fft_per_ch_per_slice(dt_feedback_true, window_size=200, step=200)
#fft_per_ch_per_slice_ev <- as.data.frame(fft_per_ch_per_slice_ev)
#fft_per_ch_per_slice_ev$label <- TrainLabels$Prediction[1]


fft_positive_feedback_800 <- c()
for(feedback in 1:length(idx_feedback_true)){
#for(feedback in 1:4){
  print(paste("feedback: ",feedback, sep=''))
  dt_feedback_true <- Data_S02_Sess01[idx_feedback_true[feedback]:(idx_feedback_true[feedback]+799),]
  fft_per_ch_per_slice_ev <- fft_per_ch_per_slice(dt_feedback_true, window_size=200, step=200)
  fft_positive_feedback_800 <- rbind(fft_positive_feedback_800,fft_per_ch_per_slice_ev)
}

fft_positive_feedback_800_lbled <- as.data.frame(fft_positive_feedback_800)
colnames(fft_positive_feedback_800_lbled) <- paste("A_",1:length(colnames(fft_positive_feedback_800_lbled)),sep="")
#labels <- rep(TrainLabels$Prediction[1:60],each=4)
#fft_positive_feedback_800_lbled$label <- labels

melted_fft_pos_feedback <- melt(data=fft_positive_feedback_800_lbled)
#melted_fft_pos_feedback$channels <- rep

#ggplot(fft_positive_feedback_800_lbled, 
#       aes(x=fft_positive_feedback_800_lbled$A_1,y=fft_positive_feedback_800_lbled$A_2))+geom_line()+theme_bw()

list.files()