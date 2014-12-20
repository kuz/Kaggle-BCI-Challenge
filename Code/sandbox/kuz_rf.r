#
# Template for hold-out-subjects cross-validation. You need to change 5 things here.
#

library('randomForest')
library('pROC')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2sec'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 2) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['ntree']] <- c(200, 500, 1000)
parameters[['mtry']] <- c(10, 30, 70, 100)
parameters[['nodesize']] <- c(1, 5, 10, 20)

# initalize parameter search grid
results <- buildgrid(parameters)

# loop over all combinations of parameters
for (r in 1:nrow(results)) {
  
  # display progress
  cat('Set of parameters', r, 'out of', nrow(results), '\n')
  cat('------------------------------\n')
  
  # read in current parameter set
  p <- results[r, ]
  
  # here we store scores for current parameter set
  scores <- c()
  
  # loop over cross-validation (training, validation) pairs
  for (cvpair in dataset$cvpairs) {
    
    # 3) PRODUCE AN OBJECT classifier HERE
    classifier <- randomForest(class ~ ., data=cvpair$train, ntree=p$ntree, mtry=p$mtry, nodesize=p$nodesize,
                               do.trace=T)
    
    # made a prediciton on a validation set
    predicted.prob <- predict(classifier, newdata=cvpair$valid, type='prob')[,2]
    
    # add record to results table
    scores <- append(scores, as.numeric(roc(cvpair$valid$class, predicted.prob)$auc))
  }
  
  # store the average score for this set of parameters
  results[r, 'score'] <- mean(scores)
  
}

# 4) SPECIFY THE METHOD YOU USED FOR THE HISTORY
mlmethod <- 'rf'

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
classifier <- randomForest(class ~ ., data=dataset$train, ntree=p$ntree, mtry=p$mtry, nodesize=p$nodesize, do.trace=T)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")[,2]
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


