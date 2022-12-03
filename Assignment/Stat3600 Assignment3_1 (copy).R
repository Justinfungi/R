#Q3
#a
data <- read.csv("data_folder/A3Q4.csv",header = TRUE)
data

y1 <- data[,1][1:9]
y2 <- data[,2]
y3<- data[,3][1:7]
y3
mean(y1)
mean(y2)
mean(y3)

totalmean <- (9*mean(y1) + 11*mean(y2) + 7*mean(y3))/279
totalmean
