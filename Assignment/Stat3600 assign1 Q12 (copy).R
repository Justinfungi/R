#Q12


Y <- c(3.3,2.5,4.7,0.25,3.0,1.03,1.75,8)
X1 <- c(34,47,25,56,29,62,41,24)
mean(X1)
mean(Y)
n <- 8
#a)
plot(X1, Y)

#b)
regression <- lm(Y~X1)
summary(regression)
abline(regression)


#c)
print("Conduct hypothesis testing")
coef(regression)

SSE <- sum((fitted(regression) - Y)^2)
SSE
sigma_2 = SSE/(8-2)
sigma_2
Sxx <- sum((X1 - mean(X1))^2)
Sxx
Syy <- sum((Y - mean(Y))^2)
Syy
S

#c)
x=0
y <- -0.09375581+0.40710656*x
y

#d)
print("i think answer in c is not reasonable since the value of itme could not be 0")
print(" Therefore, i think the key is to remove the y-intersect")
print("we can diretly set the y-intersect to 0. New Model : y = 0.40710656*x")

#e)