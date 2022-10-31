data <- read.csv("cinema.csv",header = TRUE)

Y <- data[,1]
X1 <- data[,2]
X2 <- data[,3]
X3<- data[,4]
regression <- lm(Y~X1+X2+X3)
summary(regression)

#matrix method
X <- cbind(matrix(1,10,1),X1,X2,X3)
solve(t(X)%*%X)
SSE <- sum(regression$residuals^2)
s <- sqrt(SSE/6)
s
