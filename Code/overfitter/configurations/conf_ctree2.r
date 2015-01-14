#
# Overfitter configuration for Conditional Inference Tree
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
mlmethod <- 'ctree2'

# list of parameters
parameters <- list()
parameters[['maxdepth']] <- c(1, 2, 5, 10, 20, 30)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  tunegrid <- data.frame(maxdepth=p$maxdepth)
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'ctree2', trControl=trcontrol, tuneGrid=tunegrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}