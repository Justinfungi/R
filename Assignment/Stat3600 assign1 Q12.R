#Q12
data <- read.csv("Q12.csv",header = TRUE)
data <- data[,1:2]
data

Y <- data[,2]
X1 <- data[,1]
n <-18
#a)
plot(X1, Y)

#b)
regression <- lm(Y~X1)
summary(regression)
abline(regression)


# for c,d, e

mean(X1)
coef(regression)
SSE <- sum((fitted(regression) - Y)^2)
SSE
sigma_2 = SSE/(18-2)
sigma_2
sigma_1 = sqrt(sigma_2)
sigma_1
Sxx <- sum((X1 - mean(X1))^2)
Sxx
Syy <- sum((Y - mean(Y))^2)
Syy
qt(0.975,16)

#d)
x0<- 100
l_hat <- coef(regression)[1]+ coef(regression)[2]*x0
l_hat
Sec_Term <- qt(0.975,16) * sigma_1 * (1/n+(x0-mean(X1))^2/Sxx)^(1/2)
Sec_Term
CI <- c(l_hat-Sec_Term,l_hat+Sec_Term)
CI

#e)
x0<- 100
l_hat <- coef(regression)[1]+ coef(regression)[2]*x0
l_hat
Sec_Term <- qt(0.975,16) * sigma_1 * (1/n+(x0-mean(X1))^2/Sxx+1)^(1/2)
Sec_Term
CI <- c(l_hat-Sec_Term,l_hat+Sec_Term)
CI

#f)
sigma_1
Xf <-matrix(c(1,1,100,40),2,2)
a <- matrix(c(1,-1))
Xf
a
c <- t(a) %*% Xf
c
c.T = t(c)
c.T
se <- sigma_1 * (c %*%solve(t(Xf) %*% Xf)  %*% c.T+2)^(1/2)
se
qt(0.95,df=16)
