#Q3
#a
data <- read.csv("data_folder/A4Q1.csv",header = TRUE)
data
X <-data[,1:5]
X <-matrix(unlist(X), ncol=5)
y <-data[,6]
X

model <- lm(y~X)
summary(model)

SST <- sum((y-mean(y))^2)
SST
c<-matrix(1, 34, 1)
model_none <- lm(y~c)
summary(model_none)
SSE_none <- sum(model_none$residuals^2)
SSE_none
