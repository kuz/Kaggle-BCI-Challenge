#
#  Overfitter configuration tester
#  run it at least once in each configuration file to make sure there are no mistakes in processing pipeline
#

# configuration to test
cf = './configuration/conf_bayesglm.r'



# ------- In happy circumstances you should not look below this line ------- #

# R package library location
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
source('functions.r')

# general parameters
options(width=200)

# load dataset 
datafolder <- 'fft_cz1300ms'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# load configuration  
source(cf)

# initalize parameter search grid and take only the first set
results <- buildgrid(parameters)
p <- results[1, ]

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

print(scores.in)
print(scores.out)

cat('------------------------------------------------------------------------------------------\nIf you see this line and no warnings then most probably your configuration works correctly\n------------------------------------------------------------------------------------------\n')