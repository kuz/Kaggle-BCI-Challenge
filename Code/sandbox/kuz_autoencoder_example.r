#
#  Investigating 'autoencoder' package
#

library('datasets')
library('autoencoder')

# preprare dataset
features <- as.matrix(iris[, 1:4])
labels <- iris[, 5]

# centralize data
mu <- colMeans(features)
features <- sweep(features, 2, mu, '-')

# configure the paramteres
nl = 3                          # number of layers (default is 3: input, hidden, output)
unit.type = "logistic"          # activation function ("logistic" or "tanh")
Nx.patch = 4                    # width of training image patches, in pixels
Ny.patch = 1                    # height of training image patches, in pixels
N.input = Nx.patch * Ny.patch   # number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 2                    # number of units in the hidden layer
lambda = 0.000001               # weight decay parameter
beta = 0.0001                   # weight of sparsity penalty term
rho = 0.01                      # desired sparsity parameter
epsilon <- 0.001                # a small parameter for initialization of weights
max.iterations = 2000           # number of iterations in optimizer

# run the algorithm
autoencoder <- autoencode(X.train=features,
                          nl=nl, N.hidden=N.hidden, unit.type=unit.type, lambda=lambda,
                          beta=beta, rho=rho, epsilon=epsilon, optim.method="BFGS", 
                          max.iterations=max.iterations, rescale.flag=TRUE, rescaling.offset=0.001)

# visualize hidden units
#visualize.hidden.units(autoencoder, Nx.patch, Ny.patch)

# predict features back to see how well we've learned them
pred <- predict(autoencoder, X.input=features, hidden.output=F)
predfeatures <- pred$X.output
pred$mean.error
par(mfrow=c(2,2))
for (f in 1:4) {
    plot(features[,f], type='l', col='green')
    lines(pred$X.output[,f], type='l', col='blue')
}

# extract the features
newfeatures <- as.matrix(features) %*% t(autoencoder$W[[1]]) + autoencoder$b[[1]]
newdataset <- cbind.data.frame(newfeatures, labels)

# reconstruct old features from new for a sanity check
recfeatures <- as.matrix(newfeatures) %*% t(autoencoder$W[[2]]) + autoencoder$b[[2]]
recdataset <- cbind.data.frame(recfeatures, labels)

# seems to be same up to scaling
cor(features[,1], recfeatures[,1])
cor(features[,2], recfeatures[,2])
cor(features[,3], recfeatures[,3])
cor(features[,4], recfeatures[,4])


# compare models trained on original vs. autoencoded features
library('RWeka')
names(newdataset) <- c('one', 'two', 'Species')
iris$Species <- as.factor(iris$Species)

idx <- sample(1:nrow(iris), nrow(iris) * 0.8)
iris.train <- iris[ idx, ]
iris.test  <- iris[-idx, ]
auto.train <- newdataset[ idx, ]
auto.test  <- newdataset[-idx, ]

iris.fit <- J48(Species ~., data=iris.train)
auto.fit <- J48(Species ~., data=auto.train)

iris.pred <- predict(iris.fit, iris.test)
auto.pred <- predict(auto.fit, auto.test)

sum(iris.test$Species == iris.pred) / length(iris.pred)
sum(auto.test$Species == auto.pred) / length(auto.pred)









