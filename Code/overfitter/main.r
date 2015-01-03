#
#  The main loop of The Overfitter
#  - loads configurations
#  - runs them
#  - compares predictions
#  - suggests the models for the ensemble
#

# R package library location
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
source('functions.r')

# general parameters
options(width=200)

# load dataset 
datafolder <- 'fft_cz1300ms'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# list configurations
conf.files <- dir('./configuration', pattern='conf_.*\\.r', full.names=T)

# here we store all data about all models
models <- list()

# for each configuration
for (cf in conf.files) {
  
  # load configuration  
  source(cf)

  # choose a model using cross-validation
  source('worker.r')

  # make predictions on test data using the best set of parameters
  models[[mlmethod]] <- list('p'=p, 'classifier'=classifier, 'predicted'=predicted)
  
}

# keep only models with out-of-sample score larger than 90% of maximal
scores <- sapply(models, function(x) { x$p$outscore })
print(scores)
threshold <- max(scores) * 0.9
models <- lapply(models, function(x) { if (x$p$outscore > threshold) { x } })
models <- models[ ! sapply(models, is.null) ]

# cross-correlate predictions of those models
predictions <- sapply(models, function(x) x$predicted)
correlations <- abs(cor(predictions))
print(correlations)

# pick N least correlated models
bestmodel <- names(which.max(scores))
picked <- c(bestmodel)
for (i in 1:2) {
  
  # pick a model which has least total correlation with already selected ones
  nrows <- dim(as.data.frame(correlations[picked, !colnames(correlations) %in% picked]))[2]
  if (nrows > 1) {
    unpicked <- colMeans(correlations[picked, !colnames(correlations) %in% picked])
  } else {
    unpicked <- correlations[picked, !colnames(correlations) %in% picked]
  }
  
  # and add it to the list
  picked <- append(picked, names(which.min(unpicked)))
  
}

# it might happen that same model is selected twice
picked <- unique(picked)
print(picked)

# combine their predictions into one
models <- models[picked]
predicted <- do.call('rbind.data.frame', lapply(models, function(x) { x$predicted }))
predicted <- as.numeric(colMeans(predicted))

# store the predictions make by the ensemble
result <- data.frame(read.table('results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('results/subX_', datafolder, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)