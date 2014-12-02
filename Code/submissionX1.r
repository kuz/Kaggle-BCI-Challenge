#
#  Train Naive-Bayes classifier on the dataset where 4 seconds after the feedback onset are
#  taken and FFT is done on this chunk with 1 sec step and 1 sec window
#

library('caret')

# load data
dataset.orig <- read.table('../Data/FFT Matlab/train_fft_ps4sec_win1_step1_pca99.csv', sep=',')
dataset.orig[, ncol(dataset.orig)] <- as.factor(dataset.orig[, ncol(dataset.orig)])

# split into train and validation set
train.idx <- sample(seq(along = dataset.orig[ , ncol(dataset.orig)]), nrow(dataset.orig) * 2/3)
train <- dataset.orig[ train.idx, ]
valid <- dataset.orig[-train.idx, ]

# train a model
trcontrol <- trainControl(method='cv', number=10, classProbs=T, summaryFunction=twoClassSummary)
#preproc   <- c('center', 'scale')
classifier <- train(train[,-ncol(train)], train[,ncol(train)], 'glm', trControl=trcontrol)

# see results
classifier
confusionMatrix(classifier)

# Squeeze predictions on windows back to preditions on instances
predicted   <- predict(classifier, train[, -ncol(train)])
predicted.w <- as.numeric(predicted) - 1
predicted.w <- split(predicted.w, ceiling(seq_along(predicted) / 4))
predicted.s <- rep(0, 1, length(predicted.w))

true   <- train[, ncol(train)]
true.w <- as.numeric(true) - 1
true.w <- split(true.w, ceiling(seq_along(true) / 4))
true.s <- rep(0, 1, length(true.w))

for (i in c(1:length(predicted.s))) {
  predicted.s[i] <- as.numeric(mean(predicted.w[[i]]) >= 0.5)
  true.s[i]      <- as.numeric(mean(true.w[[i]]) >= 0.5)
}

# in-sample binary classification error
e.in <- sum(predicted.s != true.s) / nrow(train)

# out-of-sample binary classification error
e.in <- sum(predicted.s != train.labels[, 2]) / length(true)
