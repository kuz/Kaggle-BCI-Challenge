#
#  Train a classifier on META data only.
#  Should be an informative result.
#

library('caret')

# load some dataset with meta data
datafolder <- 'cz2secmeta'
dataset.old <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# now use take only the meta data
dataset <- dataset.old
dataset$train <- dataset$train[,c(261:265)]
dataset$test <- dataset$test[,c(261:264)]

# train a classifier
gbmGrid <-  expand.grid(interaction.depth=1, n.trees=100, shrinkage=0.05)
trcontrol <- trainControl(method='none')
classifier <- train(class ~., data = dataset$train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep=',', header=T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_meta_gbm.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)

