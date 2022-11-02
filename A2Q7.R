#Q12
#a)
data <- read.csv("data_folder/A2Q7.csv",header = TRUE)
data
y <- data[,1]
y
x1 <- data[,2]
x1
x2 <- data[,3]
length(x2)
x3<- data[,4]
x4<- data[,5]

plot(x1, y)
plot(x2, y)
plot(x3, y)
plot(x4, y)

#b)
model <- lm(y~x1+x2+x3+x4)
summary(model)

SST <- sum((y-mean(y))^2)
SST
SSE_full <- sum(model$residuals^2)
SSE_full
MSE_full <- SSE_full/(150-4-1)
MSE_full
SSR_full <- SST-SSE_full
MSR <- SSR_full/3

model_reduce <- lm(y~x1+x2+x3)
summary(model_reduce)

SSE_reduce <- sum(model_reduce$residuals^2)
MSE_reduce <- SSE_reduce/(150-4-1)
SSR_reduce <- SST-SSE_reduce
SSR_reduce
SSEXT = SSE_reduce - SSE_full
SSEXT
MSEXT = SSEXT/1
print(paste(SSR_reduce,3))
print(paste(SSEXT,1,MSEXT))
print(paste(SSE_full,145,MSE_full))
print(paste(SST,149))
print(paste("F-ratio", MSEXT/MSE_full))

#return quantile



#d
X <- cbind(x1,x2,x3,x4)
X
solve(t(X)%*%X)
t(X)%*% y

beta <- solve(t(X)%*%X) %*% t(X)%*% y
beta

#e
X0 <- matrix(c(2000,0.5,2,8),1,4)
X0 %*% beta

qt(0.975,146)
X
solve(t(X)%*%X)
XTX_1 <- solve(t(X)%*%X)

SSE_d = (y - X %*% beta)**2
MSE_d <- sum(SSE_d)/146

a <- X0 %*% XTX_1 %*% t(X0)
a
se <- sqrt( MSE_d * (a+1))
se

#CI
X0 %*% beta
X0 %*% beta - qt(0.975,146)*se
X0 %*% beta + qt(0.975,146)*se


#f
qt(1-0.05/(2*4),146)
sqrt(4*qf(0.95,4,150-4))

c1 <- matrix(c(1,0,0,0),1,4)
c2 <- matrix(c(0,1,0,0),1,4)
c3 <- matrix(c(0,0,1,0),1,4)
c4 <- matrix(c(0,0,0,1),1,4)
g <- matrix(c(c1,c2,c3,c4),4,4)
a <- g * XTX_1 * t(g)
a<- diag(a)

se <- sqrt(MSE_d * (a))
se

beta - qt(1-0.05/(2*4),146)*se
beta + qt(1-0.05/(2*4),146)*se

#g)

SST <-sum((y-mean(y))^2)
SST

X1234<- cbind(x1,x2,x3,x4)
beta1234 <- solve(t(X1234)%*%X1234) %*% (t(X1234)%*% y)
beta1234
SSE_1234 = (y - X1234 %*% beta1234)**2
sum(SSE_1234)
MSE_1234<- sum(SSE_1234)/146
MSE_1234

X123 <- cbind(x1,x2,x3)
beta123 <- solve(t(X123)%*%X123) %*% (t(X123)%*% y)
beta123
SSE_123 = (y - X123 %*% beta123)**2
sum(SSE_123)

MSEXT <- sum(SSE_123) - sum(SSE_1234)

SSR_123 <-SST - sum(SSE_123)
SSR_123

MSEXT/MSE_1234

#i)
C <- matrix(c(43562,-1,0,0,0,0,1,-1),4,2)
a <- t(C) %*% beta
b <- solve(t(C) %*% solve(t(X1234)%*%X1234) %*% C)
c = MSE_1234
d <- t(a) %*% b %*% a
F_ratio <- (d/2)/c
F_ratio

1-pf(5.0048,2,146)
