#
# Overfitter configuration for Regularized Random Forest (RRF)
#

# packages to use during learning (loaded into each cluster node)
packages=c('pROC')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
    library(pkg, character.only=T)
}

# name of the learning method
mlmethod <- 'glm'

# list of parameters
parameters <- list()
parameters[['family']] <- c('binomial')

# Wrapper for training a classifier
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
    classifier <- glm(class ~ ., data=trainingset, family=p$family)
    return(classifier)
}

# Wrapper for predicting using trained model
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
    predicted <- predict(classifier, newdata=validset, type='response')
    return(predicted)
}