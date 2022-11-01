data <- read.csv("data_folder/NBA.csv",header = TRUE)
y <- data[,2]
x1 <- data[,3]
x2<- data[,4]
x3<- data[,5]
x4 <- data[,6]
model <- lm(y~x1+x2+x3+x4)
summary(model)

modeld <- lm(y~x3)
summary(modeld)

#################

SSE <- sum(model$residuals^2)
SST <- sum((y-mean(y))^2)
MSE <- SSE/25

X <- cbind(matrix(1,30,1),x1,x2,x3,x4)
solve(t(X)%*%X)

SSR <- SST-SSE
MSR <- SSR/4

R_square <- 1-SSR/SST
R_square
