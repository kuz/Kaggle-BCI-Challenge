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
.libPaths('/home/leontjeva/R/x86_64-unknown-linux-gnu-library/3.0')

# 2) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- '1to5butterEye8ch1300ms80pca'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 3) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'rf'

# 4) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['ntree']] <- c(500)
parameters[['mtry']] <- c(100)
parameters[['nodesize']] <- c(100)

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

# measure time
timestart <- Sys.time()

# configure parallel foreach execution
ncores <- floor(detectCores() * 0.5)  # take 1/3 of available processors
cl <- makeCluster(ncores)
registerDoParallel(cl)

# initalize parameter search grid
results <- buildgrid(parameters)

# read in current parameter set
p <- results[1, ]


# loop over cross-validation (training, validation) pairs
scores <- foreach(cv = 1:length(dataset$cvpairs), .packages=packages) %dopar% {
    
    # take cv pair
    cvpair <- dataset$cvpairs[[cv]]
    
    # train a model
    classifier <- buildmodel(p, cvpair$train)
    
    # make a prediciton on a validation and training sets
    predicted.prob.out <- makeprediction(classifier, cvpair$valid)
    predicted.prob.in <-  makeprediction(classifier, cvpair$train)
    
    # add record to results table
    if (is.na(predicted.prob.out[1])) {
        cat('WARNING: Was not able to predict probabilities. Deal with it.')
        score.out <- -1
        score.in <- -1
    } else {
        score.out <- as.numeric(roc(cvpair$valid$class, predicted.prob.out)$auc)
        score.in  <- as.numeric(roc(cvpair$train$class, predicted.prob.in)$auc)
    }
    
    #data.frame('in-score'=score.in, 'out-score'=score.out)
    list('cv'=cv, 'in-score'=score.in, 'out-score'=score.out, 'predicted'=predicted.prob.out)
}

# stop parallel processing cluster
stopCluster(cl)

# Tell how long the whole process took
print(Sys.time() - timestart)

# intialize results array
result <- data.frame(read.table('../../Data/TrainLabels.csv', sep = ',', header = T))
subjectlist <- data.frame(read.table('../../Data/train_subject_list.csv', sep = ',', header = F))
subjects <- as.numeric(unique(subjectlist)$V1)

# glue scores predicted in cv rounds together
points <- data.frame()
for (cv in 1:16) {
    idx <- which(subjectlist == subjects[cv])
    result$Prediction[idx] <- scores[[cv]]$predicted
    points <- rbind.data.frame(points, c(scores[[cv]][['in-score']], scores[[cv]][['out-score']]))
}

# show results
colnames(points) <- c('in-score', 'out-score')
print(points)
print(colMeans(points))

# store predictions on the training dataset
write.table(result, paste('../../Data/fusion/train_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)

# build final classifier
classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
predicted <- makeprediction(classifier, dataset$test)
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Data/fusion/test_', datafolder, '_', mlmethod, '.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)
