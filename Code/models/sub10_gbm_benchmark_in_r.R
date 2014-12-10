#
#  Train Random Forest classifier with undersampling on the dataset where 4 seconds 
#  after the feedback onset are taken and FFT is done on this chunk with 1 sec step 
#  and 1 sec window
#

library('caret')
library('gbm')
# load data
dataset.orig <- read.table('../Data/data.new/train_cz.csv', sep=',', header = T)
dataset.orig <- dataset.orig[,-1]
dataset.orig <- cbind(dataset.orig, 'class' = read.table('../Data//TrainLabels.csv', sep=',', header = T)[,2])
dataset.orig[, ncol(dataset.orig)] <- as.factor(dataset.orig[, ncol(dataset.orig)])
dataset.orig$class <- as.factor(ifelse(dataset.orig$class==1,"positive","negative"))

# split into train and validation set
train.idx <- sample(nrow(dataset.orig), nrow(dataset.orig) * 2/3)
train <- dataset.orig[ train.idx, ]
valid <- dataset.orig[-train.idx, ]
#train <- dataset.orig
# train a model
#gbmGrid <- expand.grid(n.trees = 500, interaction.depth = 1, shrinkage = 0.05)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (5:10)*50,
                        shrinkage = 0.05)
trcontrol <- trainControl(method='cv', number = 10, classProbs=T, summaryFunction=twoClassSummary)
classifier <- train(class ~., data = train, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

#classifier <- gbm(class ~., data = train, n.trees = 500,
#                  interaction.depth = 1, shrinkage = 0.05, n.minobsinnode = 2, cv.folds = 3)
# using canonical random forest function
#classifier <- randomForest(data=train, class ~ ., do.trace=TRUE)

# calculate probabilities
#best.iter <- gbm.perf(classifier, method="cv")
predicted.prob <- predict(classifier, newdata = valid, type = 'prob')[,1]

# see results
roc(valid$class, predicted.prob)

# Load test data
test <- read.table('../Data/data.new/test_cz.csv', sep=',', header = T)[,-1]
head(test)

predicted   <- predict(classifier, newdata = test, type="prob")$positive

result <- data.frame(read.table('../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = predicted
write.table(result, '../Results/sub10_gbm_benchmark_in_r.csv', sep = ',', quote = F, row.names = F, col.names = T)
