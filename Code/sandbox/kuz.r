#
#  Sandbox
#

library('caret')
library('data.table')

# load data
train.path <- '../../Data/train'
test.path <- '../../Data/test'
train.files <- dir(train.path, pattern='Data.*\\.csv', full.names=T)
test.files <- dir(test.path, pattern='Data.*\\.csv', full.names=T)
train.tables <- lapply(train.files, fread)
test.tables <- lapply(test.files, fread)
train.orig <- do.call(rbind, train.tables)
test.orig <- do.call(rbind, test.tables)

# extract 2 seconds after the feedback
extract <- function(dataset) {
  fb.idx <- which(dataset$FeedBackEvent == 1)
  result <- data.frame()
  for (fbi in fb.idx) {
    result <- rbind.data.frame(result, dataset[fbi:(fbi + 399), Cz])
  }
  return(result)
}
train.orig <- extract(train.orig)
test.orig <- extract(test.orig)

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

