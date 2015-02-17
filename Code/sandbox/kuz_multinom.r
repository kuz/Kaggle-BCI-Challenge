#
# Template for hold-out-subjects cross-validation. You need to change 6 things here.
#

# 1) SPECIFIFY PACKAGES TO USE DURING LEARNING HERE
# this is needed because we need to pass them to each parallel cluster separately
packages=c('pROC', 'caret')

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
datafolder <- '1to5butterEye8ch1300ms80pca_meta_5fusion'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 3) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'gbm'

# 4) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['maxit']] <- c(5, 10, 20, 50, 100, 300, 500)
parameters[['decay']] <- c(0.000001, 0.001, 0.1, 1, 10, 1000)

# 5) THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
    tunegrid <- data.frame(decay=p$decay)
    trcontrol <- trainControl(method='none')
    classifier <- train(class ~., data=trainingset, 'multinom', trControl=trcontrol, tuneGrid=tunegrid,
                        maxit=p$maxit, MaxNWts=10000)
    return(classifier)
}

# 6) THIS FUNCITON SHOULD RETURN VECTOR OF PREDICTED PROBABILITIES
# @param classifier: classifier to use to predict
# @param validset: set to validate results on
makeprediction <- function(classifier, validset) {
    predicted <- predict(classifier, newdata=validset, type='prob')$positive
    return(predicted)
}



# ------- In happy circumstances you should not look below this line ------- #

# configure parallel foreach execution
ncores <- floor(detectCores() * 0.5)
cl <- makeCluster(ncores)
registerDoParallel(cl)

# initalize parameter search grid
results <- buildgrid(parameters)

# log file name and outpur paramteres
logfile = paste('logs/', format(Sys.time(), "%Y-%m-%d-%H%M%S"), '_', datafolder, '_', mlmethod, '.txt', sep='')
options(width=200)

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
            cat('WARNING: Was not able to predict probabilities. Deal with it.')
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
sink(paste('../../Results/Summary/', datafolder, '.txt', sep=''), append=T)
print(resultlog, right=F)
cat('\n')
sink()

# choose best parameters
best.idx <- which.max(results$outscore)
p <- results[best.idx, ]

# train a model on all training data with the best parameters
#classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
#predicted <- makeprediction(classifier, dataset$test)
#result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
#result$Prediction = predicted
#write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)







