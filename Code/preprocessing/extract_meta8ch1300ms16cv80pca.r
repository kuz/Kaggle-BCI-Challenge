#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
# 
#  Channels: Cz, FC6, F5, O2, AF8, CP6, AF7, CP5
#  Time: 0-1300ms (0-260)
#  PCA on top
#  and meta data
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
train.sess <- as.numeric(substr(train.files, 35, 36))
for (i in 1:length(train.tables)) {
    train.tables[[i]] = cbind.data.frame(train.tables[[i]], 'Session'=rep(train.sess[[i]]), 
                                         'Subject'=rep(train.subjs[i], train.nrows[i]))
}
train.orig <- do.call(rbind, train.tables)

test.path <- '../../Data/raw/test'
test.files <- dir(test.path, pattern='Data.*\\.csv', full.names=T)
test.tables <- lapply(test.files, fread)
test.nrows <- sapply(test.tables, nrow)
test.subjs <- as.numeric(substr(test.files, 27, 28))
test.sess <- as.numeric(substr(test.files, 34, 35))
for (i in 1:length(test.tables)) {
    test.tables[[i]] = cbind.data.frame(test.tables[[i]], 'Session'=rep(test.sess[[i]]),
                                        'Subject'=rep(test.subjs[i], test.nrows[i]))
}
test.orig <- do.call(rbind, test.tables)

# extract new features
extract <- function(dataset) {
    counter <- 0
    fb.idx <- which(dataset$FeedBackEvent == 1)
    result <- data.frame()
    prev_sess <- -1
    for (fbi in fb.idx) {
        counter <- counter + 1
        if (prev_sess != dataset$Session[fbi]) {
            sfbcounter <- 0
            prev_sess <- dataset$Session[fbi]
        }
        sfbcounter <- sfbcounter + 1
        result <- rbind.data.frame(result, c(dataset[fbi:(fbi + 259), Cz], dataset[fbi:(fbi + 259), FC6],
                                             dataset[fbi:(fbi + 259), F5], dataset[fbi:(fbi + 259), O2],
                                             dataset[fbi:(fbi + 259), AF8], dataset[fbi:(fbi + 259), CP6],
                                             dataset[fbi:(fbi + 259), AF7],dataset[fbi:(fbi + 259), CP5],
                                             sfbcounter, dataset[fbi, Time] * 200,
                                             dataset$Session[fbi], dataset$Subject[fbi]))
        cat(counter, '/', length(fb.idx), '\r')
    }
    cat('\n')
    colnames(result)[ncol(result)-3] <- 'FeedbackNo'
    colnames(result)[ncol(result)-2] <- 'FeedbackTime'
    colnames(result)[ncol(result)-1] <- 'Session'
    colnames(result)[ncol(result)] <- 'Subject'
    return(result)
}
train.orig <- extract(train.orig)
test.orig <- extract(test.orig)

# PCA
topca <- function(train, test) {
    
    # separate brain features from meta features
    mf.train.idx <- grep("FeedbackNo|FeedbackTime|Session|Subject", colnames(train))
    mf.train <- train[,  mf.train.idx]
    bf.train <- train[, -mf.train.idx]
    
        # perform PCA on training data
        cat('Rotating ...\n')
        pca.train <- prcomp(bf.train, center=T, scale.=T)
    
    # extract transformation data
    cumvar <- cumsum(pca.train$sdev / sum(pca.train$sdev))
    keep <- which(cumvar <= 0.8)
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
    train <- cbind.data.frame(bf.train, mf.train)
    
    # separate brain features from meta features on test set
    mf.test.idx <- grep("FeedbackNo|FeedbackTime|Session|Subject", colnames(test))
    mf.test <- test[,  mf.test.idx]
    bf.test <- test[, -mf.test.idx]
    
    # move test set to PCA space
    cat('Moving test data to PCA space ...\n')
    bf.test <- sweep(bf.test, 2, center, '-')
    bf.test <- sweep(bf.test, 2, scale, '/')
    bf.test <- as.matrix(bf.test) %*% as.matrix(rotation)
    
    # drop components and glue meta data back
    bf.test <- bf.test[, keep]
    test <- cbind.data.frame(bf.test, mf.test)
    
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
colnames(train.orig) <- c(paste("A_", 1:(length(colnames(train.orig)) - 1), sep=""), 'class')
train.orig$class <- as.factor(ifelse(train.orig$class == 1, "positive", "negative"))
names(train.orig)[317:320] <- c('FeedbackNo','FeedbackTime','Session','Subject')

# prepare test set column names
colnames(test.orig) <- c(paste("A_", 1:(length(colnames(test.orig))), sep=""))
names(test.orig)[317:320] <- c('FeedbackNo','FeedbackTime','Session','Subject')

# store the resulting dataset
dataset = list('cvpairs'=cvpairs, 'test'=test.orig, 'train'=train.orig)
folder = 'meta8ch1300ms16cv80pca'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))






