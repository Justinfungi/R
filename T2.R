x <- c(34,47,25,56,29,62,41,24)
y <- c(3.3,2.5,4.7,0.25,3,1.03,1.75,8)
plot(x, y)

model <- lm(y~x)

summary(model)
