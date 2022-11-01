Q3
#a
data <- read.csv("data_folder/A2Q3.csv",header = TRUE)
data
y <- data[,1]
X1 <- data[,2]
X2 <- data[,3]
X3<- data[,4]
#X4<- data[,5]

print(t(y)%*%y - (mean(y))**2 * 7)


#b)
model <- lm(y~X1+X2+X3)
summary(model)

SST <- sum((y-mean(y))^2)
SST
SSE_full <- sum(model$residuals^2)
SSE_full
MSE_full <- SSE/
SSR_full <- SST-SSE_full
MSR <- SSR_full/3

model_reduce <- lm(y~X1+X2)
summary(model_reduce)

SSE_reduce <- sum(model_reduce$residuals^2)
MSE_reduce <- SSE_reduce/(150-4-1)
SSR_reduce <- SST-SSE_reduce

SSEXT = SSE_reduce - SSE_full
MSEXT = SSEXT/3
print(paste(SSR_reduce,1))
print(paste(SSEXT,3,MSEXT))
print(paste(SSE_reduce,145))
print(paste(SST,149))
print(paste("F-ratio", MSEXT/MSE_full))

Q12
a)
data <- read.csv("data_folder/A2Q7.csv",header = TRUE)
data
Y <- data[,1]
X1 <- data[,2]
X2 <- data[,3]
X3<- data[,4]
X4<- data[,5]

plot(X1, Y)
plot(X2, Y)
plot(X3, Y)
plot(X4, Y)

b)
model <- lm(y~x1+x2+x3+x4)
summary(model)

SST <- sum((y-mean(y))^2)

SSE_full <- sum(model$residuals^2)
MSE_full <- SSE/(150-4-1)
SSR_full <- SST-SSE_full
MSR <- SSR_full/3

model_reduce <- lm(y~x1+x2+x3)
summary(model_reduce)

SSE_reduce <- sum(model_reduce$residuals^2)
MSE_reduce <- SSE_reduce/(150-4-1)
SSR_reduce <- SST-SSE_reduce

SSEXT = SSE_reduce - SSE_full
MSEXT = SSEXT/3
print(paste(SSR_reduce,1))
print(paste(SSEXT,3,MSEXT))
print(paste(SSE_reduce,145))
print(paste(SST,149))
print(paste("F-ratio", MSEXT/MSE_full))

#return quantile

qf(0.95,3,145)
