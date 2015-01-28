# This R script is a toy example of CSP. Please let me know if you find any errors in the script at laura dot frolich at gmail dot com

# load signal processing library
library(signal)

# construct original data
x=seq(0,1000, len=200)
datnorm = rbind(sin(x/50), sin(x/25), sin(x/100), sin(x/12), -abs(x-100)/50+1,
                cos(x/50), cos(x/25), cos(x/100), cos(x/12))
datseiz = rbind(sin(x/10), sin(x/6), sin(x/25), sin(x/3), -abs(x-100)/50+1,
                cos(x/10), cos(x/6), cos(x/25), cos(x/3))
srate = 200;
period = 1/srate
dat = cbind(datnorm, datseiz)

setwd("/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data")
#---Data---#
dt_all <- readRDS("cz2secmeta/dataset.rds")

dt_positive <- t(subset(dt_all$train, class=='positive')[,1:260])
dt_negative <- t(subset(dt_all$train, class=='negative')[,1:260])
dt <- cbind(dt_positive, dt_negative)


# construct 5th order butterworth filter and filter data
bf <- butter(n=4, W=c(1, 30)/(srate), type = "pass", plane = "z")

filtereddat = matrix(nrow=dim(dt)[1], ncol=dim(dt)[2])
for(irow in 1:dim(dt)[1]){
  filtereddat[irow,] <- filter(filt=bf$b, a=bf$a, x=dt[irow,])
}

# visualize original and band-pass filtered signal
par(mfrow=c(1,2))
plot(c(0, dim(dt)[2]), c(min(dt, filtereddat), max(dt, filtereddat)), type='n')
for(irow in 1:dim(dt)[1]){
  lines(1:dim(dt)[2], filtereddat[irow,], col= "red")
  #lines(1:dim(dat)[2], dat[irow,], )
}
plot(c(0, dim(dt)[2]), c(min(dt, filtereddat), max(dt, filtereddat)), type='n')
for(irow in 1:dim(dt)[1]){
  #lines(1:dim(dat)[2], filtereddat[irow,], col= "red")
  lines(1:dim(dt)[2], dt[irow,], )
}
par(mfrow=c(1,1))

# scale and subtract mean
processeddat <- 1/sqrt(period)*filtereddat
processeddat <- processeddat - rowMeans(processeddat)

# visualize original and scaled zero-mean filtered data
plot(c(0, dim(dt)[2]), c(min(dt, processeddat), max(dt, processeddat)), type='n')
for(irow in 1:dim(dt)[1]){
  lines(1:dim(dt)[2], processeddat[irow,], col= "red")
  lines(1:dim(dt)[2], dt[irow,], )
}

# construct labels for two classes corresponding to the two
# segments in the constructed data
y = c(rep(0, times=3850), rep(1, times=1590))
normcov <- cov(t(processeddat[,1:3850]), t(processeddat[,1:3850]))
seizcov <- cov(t(processeddat[,3851:5440]), t(processeddat[,3851:5440]))

# find common spatial patterns that vary most in one condition
# and least in the other
eigsol <- eigen(solve(normcov)*seizcov)

# choose the first three and the last three spatial patters
spatpats <- cbind(eigsol$vectors[,1:100], eigsol$vectors[,161:260])

# project data onto spatial patterns
cspdat <- t(spatpats)%*%processeddat

# plot time series of original data in black
par(mfrow=c(3,1))

plot(c(0, dim(dt)[2]), c(-1, 1), type='n')
for(irow in 1:dim(dt)[1]){
  lines(1:dim(dt)[2], dt[irow,], )
}
# plot time series of data projected onto the first
# three spatial patterns in red
plot(c(0, dim(dt)[2]), c(-1, 1), type='n')
for(irow in 1){
  lines(1:dim(dt)[2], cspdat[irow,], col='red')
}
# plot time series of data projected onto the last
# three spatial patterns in blue
plot(c(0, dim(dt)[2]), c(-1, 1), type='n')
for(irow in 2){
  lines(1:dim(dt)[2], cspdat[irow,], col='blue')
}
write.table(t(cspdat), "/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Code/preprocessing/cz1300ms40csp_train.csv",
            col.names=F, row.names=F, quote=F, sep=',')

# use LDA on CSP features. CSP features are the
# logs of the variance of each time point in the
# CSP time series. Since the data is zero-mean
# the variance is the same as the value at the
# time point squared
feats <- data.frame(t(log(cspdat^2)))
res <- lm(y~X1+X2, feats)
# or lda(t(log(cspdat^2)), y)


#test data
dt_test <- t(dt_all$test[,1:260])

filtereddat_test = matrix(nrow=dim(dt_test)[1], ncol=dim(dt_test)[2])
for(irow in 1:dim(dt_test)[1]){
  filtereddat_test[irow,] <- filter(filt=bf$b, a=bf$a, x=dt_test[irow,])
}
# scale and subtract mean
processeddat_test <- 1/sqrt(period)*filtereddat_test
processeddat_test <- processeddat_test - rowMeans(processeddat_test)

cspdat_test <- t(spatpats)%*%processeddat_test
write.table(t(cspdat_test), "/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Code/preprocessing/cz1300ms200csp_test.csv",
            col.names=F, row.names=F, quote=F, sep=',')