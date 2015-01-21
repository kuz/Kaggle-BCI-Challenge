library(MASS)
library(geigen)

#common spatial pattern filter

D1  <-  read.table('../../Data/dat1.csv', sep=',')
D2  <-  read.table('../../Data/dat2.csv', sep=',')

C1 <- cov(D1)
C2 <- cov(D2)

C1 <- matrix(0, ncol=4, nrow=4)
C2 <- matrix(0, ncol=4, nrow=4)
C2[2,4] <- 1

R1 <- C1 %*% t(C1)
R1 <- R1 / sum(diag(R1))
R2 <- C2 %*% t(C2)
R2 <- R2 / sum(diag(R2))
Rsum <- R1 + R2

#Rsum <- matrix(c(0.3825,   -0.2185,    0.0688,    0.0087,
#-0.2185,    0.6525,   -0.1048,    0.2564,
#0.0688,   -0.1048,    0.2370,   -0.3548,
#0.0087,    0.2564,   -0.3548,    0.7280), nrow=4, byrow=T)

evals <- eigen(Rsum)$values
evecs <- eigen(Rsum)$vectors

W <- sqrt(ginv(diag(evals))) %*% t(evecs)
S1 <- W %*% R1 %*% t(W)
S2 <- W %*% R2 %*% t(W)

geigen(S1,S2)
