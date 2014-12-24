#
# Template for hold-out-subjects cross-validation. You need to change 6 things here.
#

# 1) SPECIFIFY PACKAGES TO USE DURING LEARNING HERE
# this is needed because we need to pass them to each parallel cluster separately
packages=c('pROC', 'randomForest')

library('foreach')
library('doParallel')
library('parallel')
source('../functions.r')
for (pkg in packages) {
  library(pkg, character.only=T)
}

# On the server the global package directory is not writable
# you might want to specify your local one here
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')

# 2) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- '8ch13sec'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 3) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'rf'

# 4) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['ntree']] <- c(200, 500, 1000)
parameters[['mtry']] <- c(10, 30, 70, 100)
parameters[['nodesize']] <- c(1, 5, 10, 20)

# 5) THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
  classifier <- randomForest(class ~ ., data=trainingset, ntree=p$ntree, mtry=p$mtry, nodesize=p$nodesize, do.trace=T)
  return(classifier)
}

# 6) THIS FUNCITON SHOULD RETURN VECTOR OF PREDICTED PROBABILITIES
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
  predicted <- predict(classifier, newdata=validset, type='prob')[,2]
  return(predicted)
}



# ------- In happy circumstances you should not look below this line ------- #

# configure parallel foreach execution
ncores <- floor(detectCores() * 0.5)
cl <- makeCluster(ncores)
registerDoParallel(cl)

# initalize parameter search grid
results <- buildgrid(parameters)

# log file name
logfile = paste('logs/', format(Sys.time(), "%Y-%m-%d-%H%M%S"), '_', datafolder, '_', mlmethod, '.txt', sep='')

# loop over all combinations of parameters
cvscores <- foreach(r = 1:nrow(results), .combine='rbind', .packages=packages) %dopar% {
  
  # read in current parameter set
  p <- results[r, ]
  
  # sink progress output to log file
  sink(logfile, append=T)
  Sys.sleep(runif(1))
  cat('Set of parameters', r, 'out of', nrow(results), '\n')
  sink()
  
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
  data.frame('score'=mean(scores), 'sd'=sd(scores))
  
}

# combine results
results$score <- cvscores$score
results$sd <- cvscores$sd

# stop parallel processing cluster
stopCluster(cl)

# store the results of this run
resultlog <- cbind.data.frame('datafolder'=rep(datafolder, nrow(results)),
                              'mlmethod'=rep(mlmethod, nrow(results)),
                              results)
sink(paste('../../Results/Summary/', datafolder, '.txt', sep=''), append=T)
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







