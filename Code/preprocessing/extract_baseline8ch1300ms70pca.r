#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
# 
#  Channels: Cz, FC6, F5, O2, AF8, CP6, AF7, CP5
#  Time: 0-1300ms (0-260)
#  Subtract baseline
#  PCA on top
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
test.nrows <- sapply(test.tables, nrow)
test.subjs <- as.numeric(substr(test.files, 27, 28))
for (i in 1:length(test.tables)) {
    test.tables[[i]] = cbind.data.frame(test.tables[[i]], 'Subject'=rep(test.subjs[i], test.nrows[i]))
}
test.orig <- do.call(rbind, test.tables)

# extract new features
extract <- function(dataset) {
    counter <- 0
    fb.idx <- which(dataset$FeedBackEvent == 1)
    result <- data.frame()
    for (fbi in fb.idx) {
        counter <- counter + 1
        
        # compute baselines
        baseline.Cz  <- mean(dataset[(fbi - 130):fbi, Cz])
        baseline.FC6 <- mean(dataset[(fbi - 130):fbi, FC6])
        baseline.F5  <- mean(dataset[(fbi - 130):fbi, F5])
        baseline.O2  <- mean(dataset[(fbi - 130):fbi, O2])
        baseline.AF8 <- mean(dataset[(fbi - 130):fbi, AF8])
        baseline.CP6 <- mean(dataset[(fbi - 130):fbi, CP6])
        baseline.AF7 <- mean(dataset[(fbi - 130):fbi, AF7])
        baseline.CP5 <- mean(dataset[(fbi - 130):fbi, CP5])
        
        # subtract baseline levels
        signal.Cz  <- dataset[fbi:(fbi + 259), Cz]  - baseline.Cz
        signal.FC6 <- dataset[fbi:(fbi + 259), FC6] - baseline.FC6
        signal.F5  <- dataset[fbi:(fbi + 259), F5]  - baseline.F5
        signal.O2  <- dataset[fbi:(fbi + 259), O2]  - baseline.O2
        signal.AF8 <- dataset[fbi:(fbi + 259), AF8] - baseline.AF8
        signal.CP6 <- dataset[fbi:(fbi + 259), CP6] - baseline.CP6
        signal.AF7 <- dataset[fbi:(fbi + 259), AF7] - baseline.AF7
        signal.CP5 <- dataset[fbi:(fbi + 259), CP5] - baseline.CP5
        
        # add an instance to the dataset
        result <- rbind.data.frame(result, c(signal.Cz, signal.FC6, signal.F5, signal.O2, signal.AF8, signal.CP6,
                                             signal.AF7, signal.CP5, dataset$Subject[fbi]))
        cat(counter, '/', length(fb.idx), '\r')
    }
    cat('\n')
    colnames(result)[ncol(result)] <- 'Subject'
    return(result)
}
train.orig <- extract(train.orig)
test.orig <- extract(test.orig)

# PCA
topca <- function(train, test) {
    
    # separate brain features from meta features
    mf.train.idx <- grep("Subject", colnames(train))
    mf.train <- train[,  mf.train.idx]
    bf.train <- train[, -mf.train.idx]
    
    # perform PCA on training data
    cat('Rotating ...\n')
    pca.train <- prcomp(bf.train, center=T, scale.=T)
    
    # extract transformation data
    cumvar <- cumsum(pca.train$sdev / sum(pca.train$sdev))
    keep <- which(cumvar <= 0.7)
    center <- pca.train$center
    scale <- pca.train$scale
    rotation <- pca.train$rotation
    
    # move training set to PCA space
    cat('Moving training data to PCA space ...\n')
    bf.train <- sweep(bf.train, 2, center, '-')
    bf.train <- sweep(bf.train, 2, scale, '/')
    bf.train <- as.matrix(bf.train) %*% as.matrix(rotation)
    
    # drop components and glue meta data back
    bf.train <- bf.train[, keep]
    train <- cbind.data.frame(bf.train, 'Subject'=mf.train)
    
    # separate brain features from meta features on test set
    mf.test.idx <- grep("Subject", colnames(test))
    mf.test <- test[,  mf.test.idx]
    bf.test <- test[, -mf.test.idx]
    
    # move test set to PCA space
    cat('Moving test data to PCA space ...\n')
    bf.test <- sweep(bf.test, 2, center, '-')
    bf.test <- sweep(bf.test, 2, scale, '/')
    bf.test <- as.matrix(bf.test) %*% as.matrix(rotation)
    
    # drop components and glue meta data back
    bf.test <- bf.test[, keep]
    test <- cbind.data.frame(bf.test, 'Subject'=mf.test)
    
    return(list('train'=train, 'test'=test))
}

cat('Performing PCA\n')
pcaset <- topca(train.orig, test.orig)
train.orig <- pcaset$train
test.orig <- pcaset$test

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

# drop subject data
train.orig$Subject <- NULL
test.orig$Subject <- NULL

# prepare train set column names and factors
train.orig[, ncol(train.orig)] <- as.factor(train.orig[, ncol(train.orig)])
colnames(train.orig) <- c(paste("A_", 1:(length(colnames(train.orig)) - 1), sep=""), 'class')
train.orig$class <- as.factor(ifelse(train.orig$class == 1, "positive", "negative"))

# prepare test set column names
colnames(test.orig) <- c(paste("A_", 1:(length(colnames(test.orig))), sep=""))

# store the resulting dataset
dataset = list('cvpairs'=cvpairs, 'test'=test.orig, 'train'=train.orig)
folder = 'baseline8ch1300ms70pca'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))






