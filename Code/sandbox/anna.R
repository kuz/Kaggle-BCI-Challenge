#libraries
library(zoo)
library(caret)
library(data.table)
library(randomForest)
library(reshape2)
library(xtable)
library(ggplot2)
library(reshape2)
library(signal)


setwd("/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data")
#---Data---#
Data_S02_Sess01 <- fread("raw/train/Data_S02_Sess01.csv")
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

TrainLabels <- fread("TrainLabels.csv")
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
    fft_per_slice <- pieces(dt[,electrode], window_size, step)
    fft_per_ch_per_slice <- cbind(fft_per_ch_per_slice, fft_per_slice)
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

#---------------------------#
#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
#

# load libraries
library('data.table')

# load data
train.path <- '../../Data/raw/train'
train.files <- dir(train.path, pattern='Data_.*\\.csv', full.names=T)
train.subjs <- as.numeric(substr(train.files, 28, 29))
train.sess <- as.numeric(substr(train.files, 35, 36))

# Labels from TrainLabels file for this session
feedback_labels <- fread('../../Data/TrainLabels.csv', sep = ',') #number of feedbacks for first subject

iter <- 1
for(subject in unique(train.subjs)){  
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

#test.path <- '../../Data/raw/test'
#test.files <- dir(test.path, pattern='Data_S04.*\\.csv', full.names=T)
#test.tables <- lapply(test.files, fread)
#test.nrows <- sapply(test.tables, nrow)
#test.subjs <- as.numeric(substr(test.files, 27, 28))
#test.sess <- as.numeric(substr(test.files, 34, 35))
#for (i in 1:length(test.tables)) {
#  test.tables[[i]] = cbind.data.frame(test.tables[[i]], 'Session'=rep(test.sess[[i]]),
#                                      'Subject'=rep(test.subjs[i], test.nrows[i]))
#}
#test.orig <- do.call(rbind, test.tables)

# Indexes of feedback event
 feedback_labels_subject <- seq(from=1, to=5440, by=340)[iter:(iter+1)]
 feedback_strt_indx <- which(train.orig$FeedBackEvent == 1)
 positive_feedback_indexes <- feedback_strt_indx[which(feedback_labels_subject$Prediction == 1)]
 negative_feedback_indexes <- feedback_strt_indx[which(feedback_labels_subject$Prediction == 0)]

# List of stimulus triggering data.frames for each feedback event in the session
 positive_window_list <- lapply(positive_feedback_indexes, function(x) as.data.frame(train.orig)[(x - 1000):(x + 1000),2:(ncol(train.orig)-2)]) #1000/200=5 sec
 negative_window_list <- lapply(negative_feedback_indexes, function(x) as.data.frame(train.orig)[(x - 1000):(x + 1000),2:(ncol(train.orig)-2)])

# Merge list of data.frame into one data.frame
#positive_window_matrix <- abind(positive_window_list, along=3)
#negative_window_matrix <- abind(negative_window_list, along=3)
#positive_window_matrix <- do.call('rbind', positive_window_list)
#negative_window_matrix <- do.call('rbind', negative_window_list)

#one_element <- c()
#for(i in 1:length(positive_window_list)){
#  one_element <- c(one_element, positive_window_list[[i]][1,1])
#}
#mean(one_element)

 positive_window_average <- Reduce("+", positive_window_list) / length(positive_window_list)
 negative_window_average <- Reduce("+", negative_window_list) / length(negative_window_list)

# Melting positive window average
 melt_positive_average <- melt(positive_window_average)
#head(melt_positive_average)

# Melting negative window average
 melt_negative_average <- melt(negative_window_average)
#head(melt_negative_average)

# Adding time for melted positive data set 
melt_positive_average$Time <- rep(as.data.frame(train.orig)[1:2001, 1], (length(train.orig[1,])-3))
melt_positive_average$Feedback_type <- as.factor(rep(1, nrow(melt_positive_average)))
#head(melt_positive_average)

# Adding time for melted negative data set 
melt_negative_average$Time <- rep(as.data.frame(train.orig)[1:2001, 1], (length(train.orig[1,])-3))
melt_negative_average$Feedback_type <- as.factor(rep(0, nrow(melt_positive_average)))
#head(melt_negative_average)

melt_average <- rbind(melt_positive_average, melt_negative_average)
#str(melt_average)

 file_name <- paste("../../Figures/stimululs_trigger_average_S0", subject, "_per_feedback_type.pdf",sep='')
 pdf(file=file_name)
 ggplot(melt_average, aes(x = Time, y = value, group = Feedback_type, colour = Feedback_type)) + 
  geom_line() + theme_bw()+
  facet_wrap(~variable)
 dev.off()
}

#------kmeans-----#
dt <- readRDS('fft_cz1300ms/dataset.rds')
#list of lists of lists
head(dt$train)


#sess <- dt$train[,c(263)] 
#subj <- dt$train[,c(264)]
signals <- dt$train[,c(1:100)]


kmeans_fit <- kmeans(signals, 5)

table(session=sess, cluster=kmeans_fit$cluster)
table(subjects=subj, cluster=kmeans_fit$cluster)

dist(kmeans_sub_2$centers)
sqrt(kmeans_sub_2$betweenss)
sqrt(kmeans_sub_2$withinss)

library(cluster) 
clusplot(signals, kmeans_fit$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)

mean_per_subject <- aggregate(data=dt$train[,c(1:260,264)], .~Subject, mean)
dist(mean_per_subject[,-1])

#-------#
dt_all <- readRDS("cz2secmeta/dataset.rds")

dt_positive <- subset(dt_all$train, class=='positive')[,1:260]
dt_negative <- subset(dt_all$train, class=='negative')[,1:260]

dt_tmp <- unlist(dt_all$train[,1:260])
dt_cut <- cbind.data.frame(original=dt_tmp, bins=cut2(dt_tmp, m=14144))

discretize <- function(data){
 dt_binned <- data
 for(i in 1:260){
   print(i)
   dt_binned[,i] <- dt_cut$bins[match(data[,i], dt_cut$original)]
 }
 dt_binned_chr <- apply(dt_binned, 2, as.character)
 return(dt_binned_chr)
}


dt_positive <- discretize(data=dt_positive) 
dt_negative <- discretize(data=dt_negative) 

hmm_discnp_pos <- hmm(dt_positive, K=5, verb=TRUE)

round(hmm_discnp_pos$tpm,3)
round(hmm_discnp_pos$ispd,3)

hmm_discnp_neg <- hmm(dt_negative, K=5, verb=TRUE)
round(hmm_discnp_neg$Rho, 3)
round(hmm_discnp_neg$tpm, 3)
round(hmm_discnp_neg$ispd, 3)
cbind(round(hmm_discnp_pos$Rho,3),round(hmm_discnp_neg$Rho, 3))

logLikHmm(dt_positive[3,],hmm_discnp_pos)
logLikHmm(dt_positive[3,],hmm_discnp_neg)

logLikHmm(dt_negative[3,],hmm_discnp_pos)
logLikHmm(dt_negative[3,],hmm_discnp_neg)

difference <- function(sequences){
  logLikHmm(sequences,hmm_discnp_pos)-logLikHmm(sequences,hmm_discnp_neg)
}
difference(dt_positive[15,]) 
difference(dt_negative[11,]) 

results_negative <- apply(dt_negative,1,difference)
results_positive <- apply(dt_positive,1,difference)
table(sign(results_negative))
table(sign(results_positive))

results <- rbind.data.frame(cbind.data.frame(dif=results_negative, class='neg'),
                            cbind.data.frame(dif=results_positive, class='pos'))

ggplot(results, aes(x=dif, color=class))+geom_density()+theme_bw()

#letters <- levels(dt_binned[,1])
#states <- c('one','two','three')
#initial <- rep(1/length(states),length(states))
#emission <- matrix(1/length(states), nrow = length(states), ncol=length(letters))
#transition <- matrix(1/length(states), nrow=length(states),ncol=length(states))
#hmm_model <- initHMM(states, letters, initial,transition,emission)
#hmm_model_bw <- baumWelch(hmm_model, dt_binned_chr[1:1000,], maxIterations = 100)