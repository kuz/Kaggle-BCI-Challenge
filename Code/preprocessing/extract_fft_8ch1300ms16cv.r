#
#  Extract cross-validation pairs (training, validation) such that division is base on subjects
# 
#  Channels: Cz, FC6, F5, O2, AF8, CP6, AF7, CP5
#  Time: 0-1300ms (0-260)
#  FFT on top
#

# load libraries
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
library('data.table')
library('zoo')

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

#
# Perform FFT on piece of signal
# @param x: signal in time domain
# @return: signal in frequency domain
#
fourier_transform_on_window <- function(x) {
    
    # parameters
    srate <- 200
    maxfreq <- srate / 2  # nyquist
    window <- 1:length(x)
    binsize <- round(length(x) / srate) * 1
    
    # detrend
    trend <- lm(x ~ window)
    detrended_data <- trend$residuals
    
    # fft
    fourier_components_norm <- abs(fft(detrended_data)) / (length(detrended_data) / 2)
    fourier_components_half <- fourier_components_norm[1:min((length(fourier_components_norm) / 2), maxfreq)]
    
    # put into 1 Hz buckets
    fourier_compoenets_buckets <- split(fourier_components_half, ceiling(seq_along(fourier_components_half) / binsize))
    fourier_compoenets_buckets <- sapply(fourier_compoenets_buckets, mean)
    fourier_compoenets_buckets <- fourier_compoenets_buckets[1:length(fourier_compoenets_buckets)-1]
    
    return(fourier_components_half)
}

# extract new features
extract <- function(dataset) {
    counter <- 0
    fb.idx <- which(dataset$FeedBackEvent == 1)
    result <- data.frame()
    for (fbi in fb.idx) {
        counter <- counter + 1
        result <- rbind.data.frame(result, c(fourier_transform_on_window(dataset[fbi:(fbi + 259), Cz]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), FC6]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), F5]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), O2]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), AF8]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), CP6]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), AF7]),
                                             fourier_transform_on_window(dataset[fbi:(fbi + 259), CP5]),
                                             dataset$Subject[fbi]))
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
folder = 'fft_8ch1300ms16cv'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))







