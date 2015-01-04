#
# Overfitter configuration for Bagged Flexible Discriminant Analysis
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
mlmethod <- 'bagFDA'

# list of parameters
parameters <- list()
parameters[['degree']] <- c(1, 2, 5)
parameters[['nprune']] <- c(NULL, 10, 50, 100)

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  tunegrid <- data.frame(degree=p$degree, nprune=p$nprune)
  trcontrol <- trainControl(method='none')
  classifier <- train(class ~., data=trainingset, 'bagFDA', trControl=trcontrol, tuneGrid=tunegrid)
  return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')$positive
  return(predicted)
}