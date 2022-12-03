#Q3
#a
data <- read.csv("data_folder/A3Q1.csv",header = TRUE)
data
y <- data[,1]
X1 <- data[,2]
X2 <- data[,3]
X3<- data[,4]
#X4<- data[,5]
model <- lm(y~X1+X2+X3)
summary(model)


#b)
mean(y)

SST <- sum((y-mean(y))^2)
SST
SSE_full <- sum(model$residuals^2)
SSE_full
SSR_full <- SST - SSE_full
SSR_full

MSR_full <- SSR_full/3
MSE_full <- SSE_full/26
MSR_full
MSE_full
F_ratio = MSR_full/MSE_full
F_ratio

qf(0.95,3,26)

#c 
model_reduce <- lm(y~X2+X3)
summary(model_reduce)

SSE_reduce <- sum(model_reduce$residuals^2)
SSE_reduce
SSEXT = SSE_reduce - SSE_full
SSEXT
MSEXT = SSEXT/1

F_ratio_c <- MSEXT/MSE_full
F_ratio_c

qf(0.95,1,26)


#d
qf(0.95,1,28)

#step1
qf(0.95,1,28)
c<-matrix(1, 30, 1)
model_none <- lm(y~c)
summary(model_none)
SSE_none <- sum(model_none$residuals^2)
SSE_none

model_x1 <- lm(y~X1)
summary(model_x1)
SSE_x1 <- sum(model_x1$residuals^2)
SSE_x1

model_x2 <- lm(y~X2)
summary(model_x2)
SSE_x2 <- sum(model_x2$residuals^2)
SSE_x2

model_x3 <- lm(y~X3)
summary(model_x3)
SSE_x3 <- sum(model_x3$residuals^2)
SSE_x3

choices <- list(SSE_x1,SSE_x2,SSE_x3)
for (x in choices) {
  #print(SSE_none)
  #print(x)
  #print(SSE_none - x)
  F_ = ((SSE_none - x)/1)/(x/(30-1-1))
  print(F_)
}

#step2
qf(0.95,1,27)
model_x12 <- lm(y~X1+X2)
summary(model_x12)
SSE_x12 <- sum(model_x12$residuals^2)
SSE_x12

model_x23 <- lm(y~X2+X3)
summary(model_x23)
SSE_x23 <- sum(model_x23$residuals^2)
SSE_x23

choices <- list(SSE_x12,SSE_x23)
for (x in choices) {
  F_ = ((SSE_x2 - x)/1)/(x/(30-2-1))
  print(F_)
}

#Step 3
qf(0.95,1,26)
model_x123 <- lm(y~X1+X2+X3)
summary(model_x123)
SSE_x123 <- sum(model_x123$residuals^2)
SSE_x123

choices <- list(SSE_x123)
for (x in choices) {
  F_ = ((SSE_x23 - x)/1)/(x/26)
  print(F_)
}


#e

#step1
qf(0.95,1,26)
model_full <- lm(y~X1+X2+X3)
summary(model_full)
SSE_full<- sum(model_full$residuals^2)
SSE_full

model_x1 <- lm(y~X3+X2)
summary(model_x1)
SSE_x1 <- sum(model_x1$residuals^2)
SSE_x1

model_x2 <- lm(y~X1+X3)
summary(model_x2)
SSE_x2 <- sum(model_x2$residuals^2)
SSE_x2

model_x3 <- lm(y~X1+X2)
summary(model_x3)
SSE_x3 <- sum(model_x3$residuals^2)
SSE_x3

choices <- list(SSE_x1,SSE_x2,SSE_x3)
for (x in choices) {
  F_ = ((x - SSE_x123)/1)/(x/26)
  print(F_)
}

#step2
qf(0.95,1,27)

model_base <- lm(y~X3+X2)
summary(model_base)
SSE_base <- sum(model_base$residuals^2)
SSE_base

model_x2 <- lm(y~X3)
summary(model_x2)
SSE_x2 <- sum(model_x2$residuals^2)
SSE_x2

model_x3 <- lm(y~X2)
summary(model_x3)
SSE_x3 <- sum(model_x3$residuals^2)
SSE_x3

choices <- list(SSE_x2,SSE_x3)
for (x in choices) {
  F_ = ((x - SSE_base)/1)/(SSE_base/27)
  print(F_)
}
