#
#  Train Random Forest classifier with undersampling on the dataset where 4 seconds 
#  after the feedback onset are taken and FFT is done on this chunk with 1 sec step 
#  and 1 sec window
#
# install.packages('ROSE')
library('caret')
library('ROSE')

# load data
dataset.orig <- read.table('../Data/FFT Matlab/fft_fb4sec_win4_step4/train_fft_fb4sec_win4_step4_pca999.csv', sep=',')
dataset.orig[, ncol(dataset.orig)] <- as.factor(dataset.orig[, ncol(dataset.orig)])
colnames(dataset.orig) <- c(paste("A_",1:(length(colnames(dataset.orig)) - 1),sep=""), 'class')
dataset.orig$class <- as.factor(ifelse(dataset.orig$class==1,"positive","negative"))
head(dataset.orig)

# standardize data
class.column <- dataset.orig[,ncol(dataset.orig)]
dataset.orig <- dataset.orig[,-ncol(dataset.orig)]
dataset.orig <- apply(dataset.orig, 2, function(x) (x - mean(x))/sd(x))
dataset.orig <- data.frame(dataset.orig, 'class' = class.column)
rm(class.column)

# split into train and validation set using undersampling
dataset.orig <- data.frame(dataset.orig, 'index' = rownames(dataset.orig))
train <- ovun.sample(formula = class~., data = dataset.orig, method = 'under', N = nrow(dataset.orig) * 2/3)

# getting rid of index column 
dataset.orig <- dataset.orig[,-ncol(dataset.orig)]
valid <- dataset.orig[-sort(as.numeric(train$data$index)), ]
train <- train$data[,-ncol(train$data)]

# split into train and validation set
#train.idx <- sample(nrow(dataset.orig), nrow(dataset.orig) * 2/3)
#train <- dataset.orig[ train.idx, ]
#valid <- dataset.orig[-train.idx, ]

# train a model
trcontrol <- trainControl(method='cv', number = 2, classProbs=T, summaryFunction=twoClassSummary)
#preproc   <- c('center', 'scale')
classifier <- train(train[,-ncol(train)], train[,ncol(train)], 'rf', do.trace = T, trControl=trcontrol)

# using canonical random forest function
#classifier <- randomForest(data=train, class ~ ., do.trace=TRUE)

# calculate probabilities
predicted.prob <- predict(classifier, newdata = valid, type="prob")[,1]

# see results
roc(valid$class, predicted.prob)

# Load test data
test <- read.table('../Data/FFT Matlab/fft_fb4sec_win4_step4/test_fft_fb4sec_win4_step4_pca999.csv', sep=',')
colnames(test) <- paste("A_",1:(length(colnames(test))),sep="")
head(test)

predicted   <- predict(classifier, newdata = test)

table(predicted)

result <- data.frame(read.table('../Results/SampleSubmission.csv', sep = ',', header = T))
result$Prediction = as.factor(ifelse(predicted == "positive",1,0))
write.table(result, '../Results/submission02_rf_on_standar_fft_win4_step4_with_under_smpl.csv', sep = ',', quote = F, row.names = F, col.names = T)
