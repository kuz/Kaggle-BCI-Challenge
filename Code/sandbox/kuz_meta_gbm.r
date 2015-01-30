#
#  Train a classifier on META data only.
#  Should be an informative result.
#

library('caret')

# load some dataset with meta data
datafolder <- 'cz2secmeta'
dataset.old <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# load last dataset with meta data
datafolder <- 'meta8ch1300ms16cv80pca'
dataset.new <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# compare the meta data which gave us 0.72 and 0.68, are there any differences?
table(dataset.new$train$FeedbackNo == dataset.old$train$FeedbackNo)
table(dataset.new$train$FeedbackTime == dataset.old$train$FeedbackTime)
table(dataset.new$train$Session == dataset.old$train$Session)
table(dataset.new$train$Subject == dataset.old$train$Subject)
table(dataset.new$train$class == dataset.old$train$class)

# now use take only the meta data
dataset <- dataset.old
dataset$train <- dataset$train[,c(261:265)]
dataset$test <- dataset$test[,c(261:264)]

# train a classifier
gbmGrid <-  expand.grid(interaction.depth=1, n.trees=500, shrinkage=0.05)
trcontrol <- trainControl(method='none')
classifier <- train(class ~., data = dataset$train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

# predict on training set
predicted   <- predict(classifier, newdata=dataset$train, type="prob")$positive
result <- data.frame(read.table('../../Data/TrainLabels.csv', sep=',', header=T))
result$Prediction = predicted
write.table(result, paste('../../Data/train_meta_predictions.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep=',', header=T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_meta_gbm.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)

