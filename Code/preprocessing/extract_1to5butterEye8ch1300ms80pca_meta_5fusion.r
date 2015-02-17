#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
# 
#  Take in ICA-eye-cleaned data
#  Channels: Cz, FC6, F5, O2, AF8, CP6, AF7, CP5
#  Time: 0-1300ms (0-260)
#

# load libraries
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
library('data.table')

# SET THIS
datafolder = '1to5butterEye8ch1300ms80pca'

# extract new features
extract <- function(dataset) {
    path <- '../../Data/fusion'
    files <- dir(path, pattern=paste(dataset, '_', datafolder, '.*\\.csv', sep=''), full.names=T)
    tables <- lapply(files, fread)
    prediction.scores <- do.call(cbind.data.frame, tables)
    prediction.scores <- prediction.scores[,c(2,4,6,8,10), with=FALSE]
    metaset <- readRDS(paste('../../Data/meta/dataset.rds', sep=''))
    result <- data.frame()
    if (dataset == 'train') {
        methods <- substr(files, start=36, stop=(nchar(files)-4))
        subjects <- fread('../../Data/train_subject_list.csv')
        result <- cbind.data.frame(prediction.scores, metaset$train[, 1:3], subjects)
        setnames(result, c(paste('prediction_score_', methods, sep=''), c('FeedbackNo', 'FeedbackTime', 'Session'), 'Subject'))
    } else if (dataset == 'test') {
        methods <- substr(files, start=35, stop=(nchar(files)-4))
        #result <- prediction.scores
        result <- cbind.data.frame(prediction.scores, metaset$test[, 1:3])
        setnames(result, c(paste('prediction_score_', methods, sep=''), c('FeedbackNo', 'FeedbackTime', 'Session')))   
    } else {
        print('Unknown dataset')
    }
    return(result)
}

train.orig <- data.frame(extract('train'))
test.orig <- data.frame(extract('test'))

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
folder = '1to5butterEye8ch1300ms80pca_meta_5fusion'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))






