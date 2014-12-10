#
#

library('randomForest')
library('pROC')

# load data
dataset <- readRDS('../../Data/cz2secmeta/dataset.rds')

# initialize results table
results <- data.frame('ntree'=numeric(), 'mtry'=numeric(), 'score'=numeric())

# loop over all possible combinations of parameters
for (ntree in c(200, 500, 800)) {
  for (mtry in c(5, 10, 20, 40)) {
    
    scores <- c()
    # loop over cross-validation (training, validation) pairs
    for (cvpair in dataset$cvpairs) {
        
      # train a classifier
      classifier <- randomForest(class ~ ., data=cvpair$train, ntree=ntree, mtry=mtry, do.trace=T)
      
      # made a prediciton on a validation set
      predicted.prob <- predict(classifier, newdata=cvpair$valid, type='prob')[,2]
        
      # add record to results table
      scores <- append(scores, as.numeric(roc(cvpair$valid$class, predicted.prob)$auc))
    }
    
    results <- rbind.data.frame(results, c(ntree, mtry, mean(scores)))
  }
}

# choose best parameters
names(results) <- c('n.trees', 'shrinkage', 'interaction.depth', 'score')
best.idx <- which.max(results$score)
n.trees <- results[best.idx, 'n.trees']
shrinkage <- results[best.idx, 'shrinkage']
interaction.depth <- results[best.idx, 'interaction.depth']

# train a classifier on whole data
gbmGrid <-  expand.grid(interaction.depth=interaction.depth, n.trees=n.trees, shrinkage=shrinkage)
trcontrol <- trainControl(method='none')
classifier <- train(class ~., data = cvpair$train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

# predict on test dataset
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, '../../Results/subX_cz2sec_meta_cv.csv', sep = ',', quote = F, row.names = F, col.names = T)








