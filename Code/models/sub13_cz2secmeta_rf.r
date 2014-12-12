#
# 
#

library('randomForest')
library('pROC')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 5) PRODUCE AN OBJECT classifier HERE (USE THE FULL TRAINING SET)
classifier <- randomForest(class ~ ., data=dataset$train, ntree=1000, mtry=100, nodesize=20, do.trace=T)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")[,2]
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


