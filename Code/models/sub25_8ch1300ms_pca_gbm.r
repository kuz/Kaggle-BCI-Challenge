#
# 
#

library('caret')
library('pROC')
source('../functions.r')

# 1) SPECIFY THE DATA FOLDER (WITH THE dataset.rds FILE PRODUCED BY ONE OF Code/preprocessing/extract_*.r SCRIPTS)
datafolder <- '8ch1300ms_pca'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# 5) PRODUCE AN OBJECT classifier HERE (USE THE FULL TRAINING SET)
gbmGrid <-  expand.grid(interaction.depth=1, n.trees=300, shrinkage=0.01)
trcontrol <- trainControl(method='none')
classifier <- train(class ~., data = dataset$train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

# predict on test dataset and store the file
predicted   <- predict(classifier, newdata=dataset$test, type="prob")$positive
result <- data.frame(read.table('../../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, paste('../../Results/subX_', datafolder, '_', mlmethod, '.csv', sep=''), sep = ',', quote = F, row.names = F, col.names = T)


