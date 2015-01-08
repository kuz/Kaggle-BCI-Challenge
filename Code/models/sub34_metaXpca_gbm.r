#
#  Average result between GBM without META (0.66) and pure META (0.67)
#  want to see correlation between the predicitons and try out what happens submit those two combined
#

dataset <- readRDS(paste('../../Data/cz2secmeta/dataset.rds', sep=''))
one <- read.table('../../Results/sub31_pca8ch1300ms16cv80pca_gbm.csv', header=T, sep=',')
two <- read.table('../../Results/sub33_meta_gbm.csv', header=T, sep=',')

# it is clear that prediction depends on session ID
plot(two$Prediction, type='l', col='blue', ylim=c(0, 1))
lines(dataset$test$Session / (5 * max(dataset$test$Session)) + 0.4, type='l', col='red')

# here is how usual prediction looks like
plot(one$Prediction, type='l', col='green', ylim=c(0, 1))
lines(two$Prediction, type='l', col='blue')

# just for fun let's try to make green line to look more like blue line

# multiply
multipled <- one$Prediction * two$Prediction * 1.3
plot(multipled, type='l', col='green', ylim=c(0, 1))
lines(two$Prediction, type='l', col='blue')

# average
averaged <- (one$Prediction + two$Prediction) / 2
plot(averaged, type='l', col='green', ylim=c(0, 1))
lines(two$Prediction, type='l', col='blue')

# store results
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = multipled
write.table(result, paste('../../Results/subX_metaXpca_gbm.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)

result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = averaged
write.table(result, paste('../../Results/subX_avgmetapca_gbm.csv', sep=''), sep=',', quote=F, row.names=F, col.names=T)
