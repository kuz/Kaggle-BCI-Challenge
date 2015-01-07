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
datafolder <- 'meta8ch1300ms16cv80pca'
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

# initalize parameter search grid
results <- buildgrid(parameters)

# read in current parameter set
p <- results[1, ]

# train a model on all training data with the best parameters
classifier <- buildmodel(p, dataset$train)

# predict on test dataset and store the file
predicted <- makeprediction(classifier, dataset$test)
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)




