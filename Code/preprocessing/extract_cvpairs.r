#
#  Sandbox
#

library('caret')
library('data.table')

# load data
train.path <- '../../Data/train'
train.files <- dir(train.path, pattern='Data.*\\.csv', full.names=T)
train.tables <- lapply(train.files, fread)
train.nrows <- sapply(train.tables, nrow)
train.subjs <- as.numeric(substr(train.files, 29, 30))
for (i in 1:length(train.tables)) {
  train.tables[[i]] = cbind.data.frame(train.tables[[i]], 'Subject'=rep(train.subjs[i], train.nrows[i]))
}
train.orig <- do.call(rbind, train.tables)

test.path <- '../../Data/test'
test.files <- dir(test.path, pattern='Data.*\\.csv', full.names=T)
test.tables <- lapply(test.files, fread)
test.orig <- do.call(rbind, test.tables)

# extract 2 seconds after the feedback
extract <- function(dataset) {
  fb.idx <- which(dataset$FeedBackEvent == 1)
  result <- data.frame()
  for (fbi in fb.idx) {
    result <- rbind.data.frame(result, c(dataset[fbi:(fbi + 259), Cz], dataset$Subject[fbi]))
  }
  colnames(result)[ncol(result)] <- 'Subject'
  return(result)
}
train.orig <- extract(train.orig)
test.orig <- extract(test.orig)

# add labels
labels <- fread('../../Data/TrainLabels.csv')
train.orig <- cbind.data.frame(train.orig, labels$Prediction[1:nrow(train.orig)])

# split into 4 pairs (training, validation)

extractpair <- function(vidx) {
  
  # extract validation and training data
  valid.idx <- subjects[ vidx]
  train.idx <- subjects[-vidx]
  valid <- subset(train.orig, Subject %in% valid.idx)
  train <- subset(train.orig, Subject %in% train.idx)
  
  # remove subject information
  valid$Subject <- NULL
  train$Subject <- NULL
  
  # modify factors and names
  valid[, ncol(valid)] <- as.factor(valid[, ncol(valid)])
  train[, ncol(train)] <- as.factor(train[, ncol(train)])
  colnames(valid) <- c(paste("A_", 1:(length(colnames(valid)) - 1), sep=""), 'class')
  colnames(train) <- c(paste("A_", 1:(length(colnames(train)) - 1), sep=""), 'class')
  valid$class <- as.factor(ifelse(valid$class == 1, "positive", "negative"))
  train$class <- as.factor(ifelse(train$class == 1, "positive", "negative"))
  
  return(list('train'=train, 'valid'=valid))
}

# extract cv pairs
subjects <- sample(unique(train.orig$Subject), 16)
cvpairs <- list()
cvpairs[[1]] <- extractpair(c(1:4))
cvpairs[[2]] <- extractpair(c(5:8))
cvpairs[[3]] <- extractpair(c(9:12))
cvpairs[[4]] <- extractpair(c(13:16))

# store the resulting dataset
dataset = list('cvpairs'=cvpairs, 'test'=test.orig)
folder = 'Cz2sec'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))

