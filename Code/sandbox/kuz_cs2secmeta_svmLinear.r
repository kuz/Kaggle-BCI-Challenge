#
# Linear SVM
#

library('caret')
library('pROC')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 2) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['C']] <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)

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
    
    # 3) PRODUCE AN OBJECT classifier HERE
    tunegrid <- data.frame(C=p$C)
    trcontrol <- trainControl(method='none', classProbs=T)
    classifier <- train(class ~., data = cvpair$train, 'svmLinear', trControl=trcontrol, tuneGrid=tunegrid)
    
    # made a prediciton on a validation set
    predicted.prob <- predict(classifier, newdata=cvpair$valid, type='prob')$positive
    
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
  
}

# 4) SPECIFY THE METHOD YOU USED FOR THE HISTORY
mlmethod <- 'svmLinear'

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

# 5) PRODUCE AN OBJECT classifier HERE (USE THE FULL TRAINING SET)
tunegrid <- data.frame(C=p$C)
trcontrol <- trainControl(method='none')
classifier <- train(class ~., data = cvpair$train, 'svmLinear', trControl=trcontrol, tuneGrid=tunegrid)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


