data <- read.csv("C:/Users/Daoyuan LAI/Dropbox/STAT3600_2021-22_2B/Tutorial/Tutorial 5/fitness_data.csv", header = TRUE)
X1 <- data[,1]
X2 <- data[,2]
Y <- data[,3]
X3 <- data[,4]
X4 <- data[,5]
X5 <- data[,6]
X6 <- data[,7]
model <- lm(Y~X1+X2+X3+X4+X5+X6)
summary(model)

qf(0.95,6,24)

sse = sum((fitted(model) - Y)^2)
sse

sst = sum((Y - mean(Y))^2)
sst


model1 <- lm(Y~X1+X3+X5+X6)
summary(model1)
ssr = sum((fitted(model1) - mean(Y))^2)
ssr

# omit many models
model2 <- lm(Y~X1+X2+X3+X5+X6)
summary(model2)
