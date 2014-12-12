#
# Template for hold-out-subjects cross-validation. You need to change 3 things here.
#

library('pROC')
library('caret')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 2) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'gbm'

# 3) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['n.trees']] <- c(10, 20)
parameters[['shrinkage']] <- c(0.01, 0.05)
parameters[['interaction.depth']] <- c(1, 2)

# 4) THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
buildmodel <- function(p, trainingset) {
  gbmGrid <-  expand.grid(interaction.depth=p$interaction.depth, n.trees=p$n.trees, shrinkage=p$shrinkage)
  trcontrol <- trainControl(method='none', classProbs=T)
  classifier <- train(class ~., data=trainingset, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)
  return(classifier)
}



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
    
    # made a prediciton on a validation set
    predicted.prob <- predict(classifier, newdata=cvpair$valid, type='prob')$positive
    
    # add record to results table
    scores <- append(scores, as.numeric(roc(cvpair$valid$class, predicted.prob)$auc))
  }
  
  # store the average score for this set of parameters
  results[r, 'score'] <- mean(scores)
  
}

# store the results of this run
resultlog <- cbind.data.frame('datafolder'=rep(datafolder, nrow(results)),
                              'mlmethod'=rep(mlmethod, nrow(results)),
                              results)
sink('../../README.md', append=T)
print(resultlog, right=F)
cat('\n')
sink()

# choose best parameters
best.idx <- which.max(results$score)
p <- results[best.idx, ]

# train a model on all training data with the best parameters
classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)







