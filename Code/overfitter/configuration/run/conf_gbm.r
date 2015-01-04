#
# Overfitter configuration for Gradient Boosting Models (GBM)
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', 'caret')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'gbm'

# list of parameters
parameters <- list()
parameters[['n.trees']] <- c(100, 300, 500)
parameters[['shrinkage']] <- c(0.0001, 0.1, 1, 1000)
parameters[['interaction.depth']] <- c(1, 2, 10)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  gbmGrid <-  expand.grid(interaction.depth=p$interaction.depth, n.trees=p$n.trees, shrinkage=p$shrinkage)
  trcontrol <- trainControl(method='none', classProbs=T)
  classifier <- train(class ~., data=trainingset, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}