#a
data <- read.csv("data_folder/A2Q8.csv",header = TRUE)
data <- data[,1:2]
data

X <- data[,1]
X
Y <- data[,2]
Y

plot(X,Y, xlab="year", ylab="surguries")

#d)
X1= X
X2 = X**2
X3 = X**3

X123<- cbind(X1,X2,X3)
X123
model123 <- lm(Y~X123)
summary(model123)
SSE_123 <- sum(model123$residuals^2)
SSE_123

X1<- cbind(X1)
X1
model1 <- lm(Y~X1)
summary(model1)
SSE_1 <- sum(model1$residuals^2)
SSE_1

a = as.numeric((SSE_1 - SSE_123))
b = as.numeric(SSE_123)

F_ratio = (a/2)/(b/8)
F_ratio

1-pf(3.224839,2,8)
