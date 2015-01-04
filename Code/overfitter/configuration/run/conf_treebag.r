#
# Overfitter configuration for Bagged CART
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
mlmethod <- 'treebag'

# list of parameters
parameters <- list()
parameters[['none']] <- c(0)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'treebag', trControl=trcontrol,
                      bagControl=bagControl(fit=ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate))
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}