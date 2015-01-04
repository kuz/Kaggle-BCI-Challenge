#
# Overfitter configuration for C5.0
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', 'C50')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'C50'

# list of parameters
parameters <- list()
parameters[['trials']] <- c(1, 10, 20, 50)
parameters[['winnow']] <- c(T, F)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- C5.0(class ~ ., data=trainingset, trials=p$trials, control=C5.0Control(winnow=p$winow))
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')[,2]
  return(predicted)
}