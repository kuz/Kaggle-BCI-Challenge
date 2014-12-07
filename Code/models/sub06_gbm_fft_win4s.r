#
#  Sandbox
#

library('caret')
library('verification')

# load data
train.orig <- read.table('../../Data/FFT Matlab/train_fft_fb4sec_win4_step4_pca99.csv', sep=',')
train.orig[, ncol(train.orig)] <- as.factor(train.orig[, ncol(train.orig)])
colnames(train.orig) <- c(paste("A_", 1:(length(colnames(train.orig)) - 1), sep=""), 'class')
train.orig$class <- as.factor(ifelse(train.orig$class == 1, "positive", "negative"))

test.orig <- read.table('../../Data/FFT Matlab/test_fft_fb4sec_win4_step4_pca99.csv', sep=',')
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

