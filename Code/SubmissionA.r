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

