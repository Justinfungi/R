matrix(1:12, nrow=3, ncol=4)
diag(3)
diag(1:3)

(A <- matrix(c(1:8,10), 3, 3))
(x <- c(1,2,3))
A %*% x # matrix multiplication
A*x

t(A) # transpose
det(A) # determinant
diag(A) # diagonal
solve(A) # inverse

cbind(A, t(A)) #stitch matrix by column
rbind(A, 1, 0)

#list
(x <- list(1:3, TRUE, "Hello", list(1:2, 5)))
x[[3]]

x[c(1,3)] #To get a sub-list, use single brackets

