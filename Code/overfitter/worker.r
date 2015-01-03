#
#  Worker who runs the cross-validation tests
#  Call to this file should be preceeded by loading a configuration
#

# configure parallel foreach execution
ncores <- floor(detectCores() * 0.5)
cl <- makeCluster(ncores)
registerDoParallel(cl)

# initalize parameter search grid
results <- buildgrid(parameters)

# log file name
logfile = paste('logs/', format(Sys.time(), "%Y-%m-%d-%H%M%S"), '_', datafolder, '_', mlmethod, '.txt', sep='')

# loop over all combinations of parameters
timestart <- Sys.time()
cvscores <- foreach(r = 1:nrow(results), .combine='rbind', .packages=packages) %dopar% {
  
  # read in current parameter set
  p <- results[r, ]
  
  # sink progress output to log file
  sink(logfile, append=T)
  Sys.sleep(runif(1))
  cat('Starting set of parameters', r, 'out of', nrow(results), '\n')
  sink()
  
  # here we store scores for current parameter set
  scores.out <- c()
  scores.in <- c()
  
  # loop over cross-validation (training, validation) pairs
  for (cvpair in dataset$cvpairs) {
    
    # train a model
    classifier <- buildmodel(p, cvpair$train)
    
    # make a prediciton on a validation and training sets
    predicted.prob.out <- makeprediction(classifier, cvpair$valid)
    predicted.prob.in <-  makeprediction(classifier, cvpair$train)
    
    # add record to results table
    if (is.na(predicted.prob.out[1])) {
      cat('WARNING: Was not able to predict probabilities. Deal with it. (', mlmethod, ')')
      scores.out <- append(scores.out, -1)
      scores.in <- append(scores.in, -1)
    } else {
      scores.out <- append(scores.out, as.numeric(roc(cvpair$valid$class, predicted.prob.out)$auc))
      scores.in  <- append(scores.in,  as.numeric(roc(cvpair$train$class, predicted.prob.in)$auc))
    }
  }
  
  # log that current set of parameters is complete
  sink(logfile, append=T)
  Sys.sleep(runif(1))
  cat('Done on set of parameters', r, 'out of', nrow(results), '\n')
  sink()
  
  # store the average score for this set of parameters
  data.frame('inscore'=mean(scores.in), 'outscore'=mean(scores.out), 'sd'=sd(scores.out))
  
}

# Tell how long the whole process took
print(Sys.time() - timestart)

# combine results
results$inscore <- cvscores$inscore
results$outscore <- cvscores$outscore
results$sd <- cvscores$sd

# stop parallel processing cluster
stopCluster(cl)

# store the results of this run
resultlog <- cbind.data.frame('datafolder'=rep(datafolder, nrow(results)),
                              'mlmethod'=rep(mlmethod, nrow(results)),
                              results)
sink(paste('results/summary/', datafolder, '.txt', sep=''), append=T)
print(resultlog, right=F)
cat('\n')
sink()

# choose best parameters
best.idx <- which.max(results$outscore)
p <- results[best.idx, ]

# train a model on all training data with the best parameters
classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
predicted <- makeprediction(classifier, dataset$test)
