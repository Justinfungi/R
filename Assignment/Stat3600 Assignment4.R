#Q3
#a
data <- read.csv("data_folder/A4Q31.csv",header = TRUE)
data
X1 <-data[,1]
y <- data[,2]
X2 <- data[,3]

X0 <- matrix(1, 24, 1)
X0
X1 <- fastDummies::dummy_cols(X1)
X1 <- X1[2:4]
X1 <-matrix(unlist(X1), ncol=3)
X<-cbind(X0, X1, X2)
colnames(X)=c(0,1,2,3,4)

X <- cbind(X[,2:3], X[,5])
X
'''
X.t <- t(X)
Hat <- solve(X.t %*% X)  %*% (X.t)
dim(Hat)
Res = Hat %*% y
Res
'''
model <- lm(y~X)
summary(model)

X[,3]
model_reduce <- lm(y~X[,3])
summary(model_reduce)

SST <- sum((y-mean(y))^2)
SST
SSE_full <- sum(model$residuals^2)
SSE_full
SSR_full <- SST - SSE_full
SSR_full

MSR_full <- SSR_full/1
MSE_full <- SSE_full/20
MSR_full
MSE_full
F_ratio = MSR_full/MSE_full
F_ratio

SSE_reduce <- sum(model_reduce$residuals^2)
SSE_reduce
SSEXT = SSE_reduce - SSE_full
SSEXT
MSEXT = SSEXT/2
MSEXT

F_ratio_c <- MSEXT/MSE_full
F_ratio_c

1-pf(129.9482,2,20)

#b)
X[,1:2]
model <- lm(y~X[,1:2])
summary(model)

c<-matrix(1, 24, 1)
model_reduce <- lm(y~c)
summary(model_reduce)

SST <- sum((y-mean(y))^2)
SST
SSE_full <- sum(model$residuals^2)
SSE_full
SSR_full <- SST - SSE_full
SSR_full

MSR_full <- SSR_full/1
MSE_full <- SSE_full/20
MSR_full
MSE_full
F_ratio = MSR_full/MSE_full
F_ratio

SSE_reduce <- sum(model_reduce$residuals^2)
SSE_reduce
SSEXT = SSE_reduce - SSE_full
SSEXT
MSEXT = SSEXT/2
MSEXT

F_ratio_c <- MSEXT/MSE_full
F_ratio_c

1-pf(16.9615,2,20)

MSE_full
c <- matrix(c(1,0,1,24),4,1)
X <- cbind(X[,1:3], X[,5])
X.t <-t(X)
cxc <- t(c) %*% solve(X.t %*% X) %*% c
se <- sqrt(MSE_full*cxc)
se
