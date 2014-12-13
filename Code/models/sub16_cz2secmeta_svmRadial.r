#
# 
#

library('caret')
library('pROC')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 5) PRODUCE AN OBJECT classifier HERE (USE THE FULL TRAINING SET)
tunegrid <- data.frame(C=1, sigma=0.001)
trcontrol <- trainControl(method='none', classProbs=T)
classifier <- train(class ~., data = trainingset, 'svmRadial', trControl=trcontrol, tuneGrid=tunegrid)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")[,2]
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


