#
# Overfitter configuration for CART
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
mlmethod <- 'rpart'

# list of parameters
parameters <- list()
parameters[['cp']] <- c(0.0001, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  tunegrid <- data.frame(cp=p$cp)
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'rpart', trControl=trcontrol, tuneGrid=tunegrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}