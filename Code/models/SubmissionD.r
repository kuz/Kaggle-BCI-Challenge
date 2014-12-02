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
train.idx <- sample(nrow(dataset.orig), nrow(dataset.orig) * 2/3)
train <- dataset.orig[ train.idx, ]
valid <- dataset.orig[-train.idx, ]

# train a model
trcontrol <- trainControl(method='cv', number = 2, classProbs=T, summaryFunction=twoClassSummary)
#preproc   <- c('center', 'scale')
classifier <- train(train[,-ncol(train)], train[,ncol(train)], 'rf', do.trace = T, trControl=trcontrol)

# using canonical random forest function
#classifier <- randomForest(data=train, class ~ ., do.trace=TRUE)

# calculate probabilities
predicted.prob <- predict(classifier, newdata = valid, type="prob")[,1]

# see results
roc(valid$class, predicted.prob)

# Squeeze predictions on windows back to preditions on instances
predicted   <- predict(classifier, valid[, -ncol(valid)])
predicted.w <- as.numeric(predicted) - 1
predicted.w <- split(predicted.w, ceiling(seq_along(predicted) / 4))
predicted.s <- rep(0, 1, length(predicted.w))

true   <- valid[, ncol(valid)]
true.w <- as.numeric(true) - 1
true.w <- split(true.w, ceiling(seq_along(true) / 4))
true.s <- rep(0, 1, length(true.w))

for (i in c(1:length(predicted.s))) {
  predicted.s[i] <- as.numeric(mean(predicted.w[[i]]) >= 0.5)
  true.s[i]      <- as.numeric(mean(true.w[[i]]) >= 0.5)
}

table(true = true.s, pred = predicted.s)

# Load test data
test <- read.table('../Data/FFT Matlab/test_fft_ps4sec_win1_step1_pca13.csv', sep=',')
colnames(test) <- paste("A_",1:(length(colnames(test))),sep="")
head(test)

predicted   <- predict(classifier, newdata = test)
predicted.w <- as.numeric(predicted) - 1
predicted.w <- split(predicted.w, ceiling(seq_along(predicted) / 4))
predicted.s <- rep(0, 1, length(predicted.w))

for (i in c(1:length(predicted.s))) {
  predicted.s[i] <- as.numeric(mean(predicted.w[[i]]) >= 0.5)
}
table(predicted.s)

result <- data.frame(read.table('../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted.s
write.table(result, '../Results/submission01_fft.csv.csv', sep = ',', quote = F, row.names = F, col.names = T)
