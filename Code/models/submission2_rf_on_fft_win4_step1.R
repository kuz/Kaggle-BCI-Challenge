#
#  Train Random Forest classifier with undersampling on the dataset where 4 seconds 
#  after the feedback onset are taken and FFT is done on this chunk with 1 sec step 
#  and 1 sec window
#

library('caret')

# load data
dataset.orig <- read.table('../Data/FFT Matlab/train_fft_ps4sec_win1_step1_pca99.csv', sep=',')
dataset.orig[, ncol(dataset.orig)] <- as.factor(dataset.orig[, ncol(dataset.orig)])
colnames(dataset.orig) <- c(paste("A_",1:(length(colnames(dataset.orig)) - 1),sep=""), 'class')
dataset.orig$class <- as.factor(ifelse(dataset.orig$class==1,"positive","negative"))
# split into train and validation set

train.idx <- sample(nrow(dataset.orig), nrow(dataset.orig)*2/3)
train <- dataset.orig[ train.idx, ]
valid <- dataset.orig[-train.idx, ]
train <- dataset.orig
# train a model
trcontrol <- trainControl(method='cv', number=2, classProbs=T, summaryFunction=twoClassSummary)
classifier <- train(train[,-ncol(train)], train[,ncol(train)], 'rf', do.trace = T, trControl=trcontrol)

predicted.prob <- predict(classifier, newdata = valid, type="prob")[,1]
roc(valid$class, predicted.prob)

# Squeeze predictions on windows back to preditions on instances
predicted.w <- split(predicted.prob, ceiling(seq_along(predicted.prob) / 4))
predicted.s <- sapply(predicted.w, FUN=mean)

true   <- valid[, ncol(valid)]
true.w <- as.numeric(true) - 1
true.w <- split(true.w, ceiling(seq_along(true) / 4))
true.s <- sapply(true.w, function(x) as.numeric(mean(x)>=0.5))

roc(true.s,predicted.s)


#read test data
dataset.orig.test <- read.table('../Data/FFT Matlab/test_fft_ps4sec_win1_step1_pca13.csv', sep=',')
#dataset.orig.test[, ncol(dataset.orig.test)] <- as.factor(dataset.orig.test[, ncol(dataset.orig.test)])
colnames(dataset.orig.test) <- paste("A_",1:length(colnames(dataset.orig.test)),sep="")
#dataset.orig.test$class <- rep(c(1,0),nrow(dataset.orig.test)/2)
predictions_on_test <- predict(classifier, dataset.orig.test, type="prob")[,1]
predicted.w <- split(predictions_on_test, ceiling(seq_along(predictions_on_test) / 4))
predicted.s <- sapply(predicted.w, FUN=mean)
predicted.plot <- as.data.frame(predicted.s)
ggplot(predicted.plot, aes(x=predicted.s))+geom_histogram()+theme_bw()
predictions_on_test_bin <- ifelse(predicted.s>=0.4,0,1)
#dataset.orig.test$class <- as.factor(ifelse(dataset.orig.test$class==1,"positive","negative"))

result <- data.frame(read.table('../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predictions_on_test_bin
write.table(result, '../Results/submission2_rf_on_fft_win4_step1.csv', sep = ',', quote = F, row.names = F, col.names = T)
