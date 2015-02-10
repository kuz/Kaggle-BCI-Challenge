# Correlation between predictions based on filtered data 1 to 5 and 5 to 10.
prediction1to5 <- read.csv('Results/subX_1to5butterEye8ch1300ms80pca_gbm.csv')
prediction5to10 <- read.csv('Results/subX_5to10butterEye8ch1300ms80pca_gbm.csv')

cor(prediction1to5[,2], prediction5to10[,2])
signalL <- length(prediction1to5[,2])
plot(seq(1,signalL,1), sort(prediction1to5[,2]), type = 'l')
lines(seq(1,signalL,1), sort(prediction5to10[,2]), col = 'red')

# Try merging those two and meta data
# predict on test dataset and store the file
predicted.meta <- read.table('Data/test_meta_predictions.csv', sep=',', header=T)
predicted.meta <- predicted.meta$Prediction

result <- data.frame(read.table('Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = (prediction1to5[,2] + prediction5to10[,2] + predicted.meta) / 3
write.table(result, paste('Results/subX_1to5and5to10butterEye8ch1300ms80pca_gbm_meta.csv', sep=''),
            sep=',', quote=F, row.names=F, col.names=T)
