#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
#

# load libraries
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
library('data.table')

# load data
train.path <- '../../Data/raw/train'
train.files <- dir(train.path, pattern='Data.*\\.csv', full.names=T)
train.tables <- lapply(train.files, fread)
train.nrows <- sapply(train.tables, nrow)
train.subjs <- as.numeric(substr(train.files, 28, 29))
for (i in 1:length(train.tables)) {
  train.tables[[i]] = cbind.data.frame(train.tables[[i]], 'Subject'=rep(train.subjs[i], train.nrows[i]))
}
train.orig <- do.call(rbind, train.tables)

test.path <- '../../Data/raw/test'
test.files <- dir(test.path, pattern='Data.*\\.csv', full.names=T)
test.tables <- lapply(test.files, fread)
test.orig <- do.call(rbind, test.tables)

# extract 2 seconds after the feedback
extract <- function(dataset) {
  counter <- 0
  fb.idx <- which(dataset$FeedBackEvent == 1)
  result <- data.frame()
  for (fbi in fb.idx) {
    counter <- counter + 1
    result <- rbind.data.frame(result, c(dataset[fbi:(fbi + 259), Cz], dataset$Subject[fbi]))
    cat(counter, '/', length(fb.idx), '\r')
  }
  cat('\n')
  colnames(result)[ncol(result)] <- 'Subject'
  return(result)
}
train.orig <- extract(train.orig)
test.orig <- extract(test.orig)

# add labels
labels <- fread('../../Data/TrainLabels.csv')
train.orig <- cbind.data.frame(train.orig, labels$Prediction[1:nrow(train.orig)])

# split into 4 pairs (training, validation)
#
# @param vidx: vector of test subject's which will go into the validation set c(1,2,3,4)
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
subjects <- sort(as.numeric(unique(train.orig$Subject)))
cvpairs <- list()
for (i in 1:16) {
  cvpairs[[i]] <- extractpair(c(i))
}

# prepare train set column names and factors
train.orig[, ncol(train.orig)] <- as.factor(train.orig[, ncol(train.orig)])
train.orig$Subject <- NULL
colnames(train.orig) <- c(paste("A_", 1:(length(colnames(train.orig)) - 1), sep=""), 'class')
train.orig$class <- as.factor(ifelse(train.orig$class == 1, "positive", "negative"))

# prepare test set column names
colnames(test.orig) <- c(paste("A_", 1:(length(colnames(test.orig))), sep=""))

# store the resulting dataset
dataset = list('cvpairs'=cvpairs, 'test'=test.orig, 'train'=train.orig)
folder = 'cz2sec_15to1'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))
















