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
datafolder <- 'baseline8ch1300ms70pca'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 3) SPECIFY THE METHOD YOU USE (NEEDED JUST FOR RECORD)
mlmethod <- 'gbm'

# 4) ENLIST PARAMETERS HERE
parameters <- list()
parameters[['n.trees']] <- c(500)
parameters[['shrinkage']] <- c(0.05)
parameters[['interaction.depth']] <- c(1)

# 5) THIS FUNCITON SHOULD RETURN classifier OBJECT
# @param p: current set of parameters
# @param trainingset: set to train model on
buildmodel <- function(p, trainingset) {
    gbmGrid <-  expand.grid(interaction.depth=p$interaction.depth, n.trees=p$n.trees, shrinkage=p$shrinkage)
    trcontrol <- trainControl(method='none', classProbs=T)
    classifier <- train(class ~., data=trainingset, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)
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

# load leacked predictions
leaked <- data.frame(read.table('../../Results/data_leak_train_predictions.csv', sep = ',', header = T))

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

# list of subjects
subjects <- c('S02_Sess05', 'S06_Sess05', 'S07_Sess05', 'S11_Sess05', 'S12_Sess05', 'S13_Sess05',
              'S14_Sess05', 'S16_Sess05', 'S17_Sess05', 'S18_Sess05', 'S20_Sess05', 'S21_Sess05', 
              'S22_Sess05', 'S23_Sess05', 'S24_Sess05', 'S26_Sess05')

# loop over cross-validation (training, validation) pairs
scores <- foreach(cv = 1:length(dataset$cvpairs), .combine='rbind', .packages=packages) %dopar% {
    
    # take cv pair
    cvpair <- dataset$cvpairs[[cv]]
    
    # train a model
    classifier <- buildmodel(p, cvpair$train)
    
    # make a prediciton on a validation and training sets
    predicted.prob.out <- makeprediction(classifier, cvpair$valid)
    predicted.prob.in <-  makeprediction(classifier, cvpair$train)
    
    # update predictions with with leaked ones
    sess.five.idx <- grep(paste(subjects[cv], '.', sep=''), leaked$IdFeedBack)
    predicted.leak <- leaked[sess.five.idx, 'Prediction']
    predicted.prob.out[241:340] <- predicted.leak
    
    # add record to results table
    if (is.na(predicted.prob.out[1])) {
        cat('WARNING: Was not able to predict probabilities. Deal with it.')
        score.out <- -1
        score.in <- -1
    } else {
        score.out <- as.numeric(roc(cvpair$valid$class, predicted.prob.out)$auc)
        score.in  <- as.numeric(roc(cvpair$train$class, predicted.prob.in)$auc)
    }
    
    data.frame('in-score'=score.in, 'out-score'=score.out)
}

# stop parallel processing cluster
stopCluster(cl)

# Tell how long the whole process took
print(Sys.time() - timestart)

# show results
print(scores)
print(colMeans(scores))





