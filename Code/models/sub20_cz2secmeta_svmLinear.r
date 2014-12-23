#
# 
#

library('caret')
library('pROC')
source('../functions.r')

# SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- svm(class ~., data=trainingset, kernel='linear', scale=p$scale, cost=p$C, probability=T)
  return(classifier)
}

# THIS FUNCITON SHOULD RETURN VECTOR OF PREDICTED PROBABILITIES
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, probability=T)
  predicted <- as.numeric(attr(predicted, "probabilities")[,'positive'])
  return(predicted)
}

# PRODUCE AN OBJECT classifier HERE (USE THE FULL TRAINING SET)
p = list(scale=T, C=0.0001)
classifier <- buildmodel(p, dataset$train)

# PREDICT ON TEST DATA HERE
predicted <- makeprediction(classifier, dataset$test)

result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


