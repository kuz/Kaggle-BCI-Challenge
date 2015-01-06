#
# Overfitter configuration for ...
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC', '...')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- '...'

# list of parameters
parameters <- list()
parameters[['...']] <- c(...)
parameters[['...']] <- c(...)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- ...
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- ...
  return(predicted)
}