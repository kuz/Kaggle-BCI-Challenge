#
#  Template for using our cross-validation scheme
#

library('gbm')
library('pROC')

# load data
dataset <- readRDS('../../Data/cz2sec/dataset.rds')

# initialize results table
results <- data.frame('n.trees'=numeric(), 'shrinkage'=numeric(), 'interaction.depth'=numeric(), 'score'=numeric())

# loop over all possible combinations of parameters
for (n.trees in c(250)) {
  for (shrinkage in c(0.05)) {
    for (interaction.depth in c(1)) {
      
      # loop over cross-validation (training, validation) pairs
      for (cvpair in dataset$cvpairs) {
        
        # train a classifier
        gbmGrid <-  expand.grid(interaction.depth=interaction.depth, n.trees=n.trees, shrinkage=shrinkage)
        trcontrol <- trainControl(method='none')
        classifier <- train(class ~., data = cvpair$train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)
        
        # made a prediciton on a validation set
        predicted.prob <- predict(classifier, newdata=cvpair$valid, type='prob')$positive
        
        # add record to results table
        results <- rbind.data.frame(results, c(n.trees, shrinkage, interaction.depth,
                                               as.numeric(roc(cvpair$valid$class, predicted.prob)$auc)))
      }
    }
  }
}