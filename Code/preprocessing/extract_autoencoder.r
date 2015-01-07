#
#  Extract features using Autoencoder
# 
#  Channels: Cz, FC6, F5, O2, AF8, CP6, AF7, CP5
#  Time: 0-1300ms (0-260)
#

# load libraries
.libPaths('/home/kuzovkin/R/x86_64-unknown-linux-gnu-library/3.0')
library('data.table')
library('autoencoder')

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
    
    # extract 1300ms after each feedback event
    counter <- 0
    fb.idx <- which(dataset$FeedBackEvent == 1)
    afterfeedback <- data.frame()
    for (fbi in fb.idx) {
        counter <- counter + 1
        afterfeedback <- rbind.data.frame(afterfeedback,
                                          c(dataset[fbi:(fbi + 259), Cz], dataset[fbi:(fbi + 259), FC6],
                                            dataset[fbi:(fbi + 259), F5], dataset[fbi:(fbi + 259), O2],
                                            dataset[fbi:(fbi + 259), AF8], dataset[fbi:(fbi + 259), CP6],
                                            dataset[fbi:(fbi + 259), AF7],dataset[fbi:(fbi + 259), CP5],
                                            dataset$Subject[fbi]))
        cat(counter, '/', length(fb.idx), '\r')
    }
    cat('\n')
    colnames(afterfeedback)[ncol(afterfeedback)] <- 'Subject'
    
    # separate brain features from meta features
    meta.idx <- grep("Subject", colnames(afterfeedback))
    features <- afterfeedback[, -meta.idx]
    meta     <- afterfeedback[,  meta.idx]
    
    # centralize data
    mu <- colMeans(features)
    features <- sweep(features, 2, mu, '-')
    
    # configure autoencoder parameters
    nl = 3                                   # number of layers (default is 3: input, hidden, output)
    unit.type = "logistic"                   # activation function ("logistic" or "tanh")
    Nx.patch = 4                             # width of training image patches, in pixels
    Ny.patch = 1                             # height of training image patches, in pixels
    N.input = Nx.patch * Ny.patch            # number of units (neurons) in the input layer (one unit per pixel)
    N.hidden = round(ncol(features) * 0.01)  # number of units in the hidden layer
    lambda = 0.000001                        # weight decay parameter
    beta = 0.0001                            # weight of sparsity penalty term
    rho = 0.01                               # desired sparsity parameter
    epsilon <- 0.001                         # a small parameter for initialization of weights
    max.iterations = 2000                    # number of iterations in optimizer
    
    # measure time
    timestart <- Sys.time()
    
    # run the algorithm
    autoencoder <- autoencode(X.train=as.matrix(features),
                              nl=nl, N.hidden=N.hidden, unit.type=unit.type, lambda=lambda,
                              beta=beta, rho=rho, epsilon=epsilon, optim.method="BFGS", 
                              max.iterations=max.iterations, rescale.flag=TRUE, rescaling.offset=0.001)
    
    # measured time
    print(Sys.time() - timestart)
    
    
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
subjects <- sample(unique(train.orig$Subject), 16)
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
folder = '8ch1300ms16cv'
system(paste('mkdir ../../Data/', folder, sep=''))
saveRDS(dataset, paste('../../Data/', folder, '/dataset.rds', sep=''))






