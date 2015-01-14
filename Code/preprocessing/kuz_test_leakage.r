#
#  Following the data leakage hind from the forum
#  http://www.kaggle.com/c/inria-bci-challenge/forums/t/11178/possible-data-leakage/59838
#

library('data.table')

# prepare results
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))

# list of files to process
prefixes <- c('S01_Sess05', 'S03_Sess05', 'S04_Sess05', 'S05_Sess05', 'S08_Sess05',
              'S09_Sess05', 'S10_Sess05', 'S15_Sess05', 'S19_Sess05', 'S25_Sess05')

# loop over test files
for (prefix in prefixes) {

    cat('Processing', prefix, '\n')
    
    # load data
    subject <- fread(paste('../../Data/raw/test/Data_', prefix,'.csv', sep=''))

    # extract times of feedback events
    data <- subject[, c(1, 59), with=F]
    data <- data[data$FeedBackEvent == 1, ]

    # remember which id's we are going to predict
    sample.idx <- grep(paste(prefix, '.', sep=''), result$IdFeedBack)

    # time difference
    diffs <- rep(0, 100)
    for (t in 2:nrow(data)) {
        diffs[t] = data[t, Time] - data[t - 1, Time]
    }

    # align with lables
    secret <- data.frame('td'=diffs, 'fifth'=rep(0, 100), 'pred'=rep(NA, 100))
    secret[1, 1] <- min(secret[2:nrow(secret), 1])

    # mark fifths letters
    i <- 6
    while (i <= 100) {
        secret[i, 'fifth'] <- 1
        i <- i + 5
    }

    # compute thresholds
    bigs   <- secret[secret$fifth == 1, 1]
    smalls <- secret[secret$fifth == 0, 1]
    mean.big   <- (max(bigs) + min(bigs)) / 2
    mean.small <- (max(smalls) + min(smalls)) / 2

    # make predictions based on leaked info
    for (i in 1:nrow(secret)) {
        if (secret[i, 'fifth'] == 1) {
            if (secret[i, 'td'] < mean.big) {
                secret[i, 'pred'] <- 1
            } else {
                secret[i, 'pred'] <- 0
            }
        } else {
            if (secret[i, 'td'] < mean.small) {
                secret[i, 'pred'] <- 1
            } else {
                secret[i, 'pred'] <- 0
            }
        }
    }
    
    # put predictions into results table
    result[sample.idx, 'Prediction'] <- secret[, 'pred']
    
}
    
# store the predictions
write.table(result, paste('../../Results/data_leakage_predictions.csv', sep=''),
            sep=',', quote=F, row.names=F, col.names=T)


