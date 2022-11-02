#Q3
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
MSE_full <- SSE_full/3
SSR_full <- SST-SSE_full
SSR_full
MSR <- SSR_full

model_reduce <- lm(y~X2)
summary(model_reduce)

SSE_reduce <- sum(model_reduce$residuals^2)
SSE_reduce
MSE_reduce <- SSE_reduce/3
SSR_reduce <- SST-SSE_reduce
SSR_reduce
SSEXT = SSE_reduce - SSE_full
MSEXT = SSEXT
print(paste(SSR_reduce,2))
print(paste(SSEXT,1,MSEXT))
print(paste(SSE_reduce,3))
print(paste(SST,6))
print(paste("F-ratio", MSEXT/MSE_full))
MSEXT
MSE_full

1-pf(46.7775,1,3)
SSE_full/(4*84)

1-pt(7.8978,4)

qf(0.95,2,26)
1-pf(1.1562,2,26)
1-pf(46.7775,1,3)
1-pt(6.839411,4)

qt(0.975,4)
qt(0.95,4)
