#
# Overfitter configuration for Bagged Model
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
mlmethod <- 'bag'

# list of parameters
parameters <- list()
parameters[['vars']] <- c(1, 10, round(0.001*nf), round(0.1*nf), nf, 100*nf)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  tunegrid <- data.frame(vars=p$vars)
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'bag', trControl=trcontrol, tuneGrid=tunegrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}