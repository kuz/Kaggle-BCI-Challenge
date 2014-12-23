#
# Linear SVM
#

library('pROC')
library('e1071')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 2) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'svmLinear'

# 3) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['C']] <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)
parameters[['scale']] <- c(T, F)

# 4) THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- svm(class ~., data=trainingset, kernel='linear', scale=p$scale, cost=p$C, probability=T)
  return(classifier)
}

# 5) THIS FUNCITON SHOULD RETURN VECTOR OF PREDICTED PROBABILITIES
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, probability=T)
  predicted <- as.numeric(attr(predicted, "probabilities")[,'positive'])
  return(predicted)
}


# --- In happy circumstances you should not look below this line --- #


# initalize parameter search grid
results <- buildgrid(parameters)

# loop over all combinations of parameters
for (r in 1:nrow(results)) {
  
  # read in current parameter set
  p <- results[r, ]
  
  # display progress
  cat('Set of parameters', r, 'out of', nrow(results), '\n')
  print(p)
  cat('------------------------------\n')
  
  # here we store scores for current parameter set
  scores <- c()
  
  # loop over cross-validation (training, validation) pairs
  for (cvpair in dataset$cvpairs) {
    
    # train a model
    classifier <- buildmodel(p, cvpair$train)
    
    # make a prediciton on a validation set
    predicted.prob <- makeprediction(classifier, cvpair$valid)
    
    # add record to results table
    if (is.na(predicted.prob[1])) {
      cat('WARNING: Was not able to predict probabilities. Deal with it.')
      scores <- append(scores, -1)
    } else {
      scores <- append(scores, as.numeric(roc(cvpair$valid$class, predicted.prob)$auc))
    }
  }
  
  # store the average score for this set of parameters
  results[r, 'score'] <- mean(scores)
  results[r, 'sd'] <- sd(scores)
  
}

# store the results of this run
resultlog <- cbind.data.frame('datafolder'=rep(datafolder, nrow(results)),
                              'mlmethod'=rep(mlmethod, nrow(results)),
                              results)
sink('../../README.txt', append=T)
print(resultlog, right=F)
cat('\n')
sink()

# choose best parameters
best.idx <- which.max(results$score)
p <- results[best.idx, ]

# train a model on all training data with the best parameters
classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
predicted <- makeprediction(classifier, dataset$test)
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)





