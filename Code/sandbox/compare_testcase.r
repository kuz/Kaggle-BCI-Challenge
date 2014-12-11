#
#  Compare the prediction from Dima's submission (0.7) and my (0.48)
#  They should be identical, so what is the difference?
#

library('caret')
library('gbm')


# Dima
# ----

# load data
dataset.orig <- read.table('../../Data/gbm_benchmark/train_cz.csv', sep=',', header = T)
dataset.orig <- dataset.orig[,-1]
dataset.orig <- cbind(dataset.orig, 'class' = read.table('../../Data/TrainLabels.csv', sep=',', header = T)[,2])
dataset.orig[, ncol(dataset.orig)] <- as.factor(dataset.orig[, ncol(dataset.orig)])
dataset.orig$class <- as.factor(ifelse(dataset.orig$class==1,"positive","negative"))

datafolder <- 'cz2secmeta'
dataset <- readRDS(paste('../../Data/', datafolder, '/dataset.rds', sep=''))

# make them identical
dataset.orig$Cz_260 <- NULL
dataset.orig <- cbind(dataset.orig[,-c(1:4,265)], dataset.orig[,c(3:4)], dataset.orig[,2],
                      dataset.orig[,1], dataset.orig[,265])
colnames(dataset.orig) <- colnames(dataset$train)
dataset.orig$FeedbackTime <- dataset.orig$FeedbackTime + 1
dataset.orig$FeedbackNo <- dataset.orig$FeedbackNo + 1


# split into train and validation set
train.idx <- sample(nrow(dataset.orig), nrow(dataset.orig) * 2/3)
trainD <- dataset.orig[ train.idx, ]
validD <- dataset.orig[-train.idx, ]
trainM <- dataset$train[ train.idx, ]
validM <- dataset$train[-train.idx, ]

# classifiers
gbmGrid <-  expand.grid(interaction.depth = 1, n.trees = 500, shrinkage = 0.05)
trcontrol <- trainControl(method='none')
classifierD <- train(class ~., data = trainD, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

gbmGrid <-  expand.grid(interaction.depth=1, n.trees=500, shrinkage=0.05)
trcontrol <- trainControl(method='none')
classifierM <- train(class ~., data = trainM, 'gbm', trControl=trcontrol, tuneGrid = gbmGrid)

# results
predicted.prob <- predict(classifierD, newdata = validD, type = 'prob')[,1]
roc(validD$class, predicted.prob)
predicted.prob <- predict(classifierD, newdata = validM, type = 'prob')[,1]
roc(validM$class, predicted.prob)
predicted.prob <- predict(classifierM, newdata = validD, type = 'prob')[,1]
roc(validD$class, predicted.prob)
predicted.prob <- predict(classifierM, newdata = validM, type = 'prob')[,1]
roc(validM$class, predicted.prob)

# $positive
predicted.prob <- predict(classifierD, newdata = validD, type = 'prob')$positive
roc(validD$class, predicted.prob)
predicted.prob <- predict(classifierD, newdata = validM, type = 'prob')$positive
roc(validM$class, predicted.prob)
predicted.prob <- predict(classifierM, newdata = validD, type = 'prob')$positive
roc(validD$class, predicted.prob)
predicted.prob <- predict(classifierM, newdata = validM, type = 'prob')$positive
roc(validM$class, predicted.prob)

#
# The error was that in my data I had a global time and occurence counter for feedback events.
# The correct way to do is to have session-specific counters
#