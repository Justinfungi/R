
data <- read.csv("Q10.csv",header = TRUE)
data <- data[,1:2]
data

Y <- data[,2]
X1 <- data[,1]
regression <- lm(Y~X1)
summary(regression)

#a)
coef(regression)

#b)
print("Conduct hypothesis testing: H0: β")
pt(0.5/2,15-2)

#c)
x=0
y <- -0.09375581+0.40710656*x
y

#d)
print("i think answer in c is not reasonable since the value of itme could not be 0")
print(" Therefore, i think the key is to remove the y-intersect")
print("we can diretly set the y-intersect to 0. New Model : y = 0.40710656*x")

#e)

Y <- matrix(c(130,147,145,146,152,154,151,159))
Y
sum(Y)
sum(Y)/8
sum(Y[1:4])
sum(Y[5:8])
-1/8*sum(Y[1:4])+sum(Y[5:8])*1/8

# Verification
X <- matrix(c(1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1),8,2)
X.t =t(X)
Hat <- solve(X.t %*% X)  %*% (X.t)
beta = Hat %*% Y

#sigma
sum((Y[1:4]-(beta[1]-beta[2]))^(2))
sum((Y[5:8]-(beta[1]+beta[2]))^(2))

# 
