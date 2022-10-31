matrix(1:12, nrow=3,ncol=4)
diag(3)
diag(1:3)

A <- matrix(c(1:8,10),3,3)
x <- c(1,2,3)

A %*% x # matrix mul
A*x

#Siimple attributes
t(A)     # Transpose
det(A)   # Determinant
diag(A)  # Diagonal
solve(A) # Find inverse matrix

# list
x <- list(1:3,TRUE,"HELLO",list(1:2,5))
x[[3]]
x[c(1,3)]

'''
cbind： 根据列进行合并，即叠加所有列，m列的矩阵与n列的矩阵cbind()最后变成m+n列，合并前提：cbind(a, c)中矩阵a、c的行数必需相符
rbind： 根据行进行合并，就是行的叠加，m行的矩阵与n行的矩阵rbind()最后变成m+n行，合并前提：rbind(a, c)中矩阵a、c的列数必需相符
'''
cbind(A, t(A)) # stith matrix by col
rbind(A,1,0)

# Normal Distribution

# Prob (The function pnorm returns the integral from −∞ to q of the pdf)
# input: how the value it is
# output: the prob below it
pnorm(q,mean=0,sd=1) 

#The qnorm function is simply the inverse of the cdf,
# input probability (AKA quantile)
# output: the value
qnorm(p,mean=0,sd=1) 
    #Z-score of the 50th quantile
qnorm(.75) # input prob, output quantile


#generate a vector of normally distributed random number
set.seed(10-1-2015)
rnorm(n,mean=0,sd=1) # randomly generate number n

# for t
qt(q.df)
pt(p,df)
rt(n,df)

# for chi-square
pchisq(p,df)
qchisq(q,df)
rchisq(n,df)

# for F
pf(p,df1,df2)
qf(q,df1,df2)
rf(n,df1,df2)

# Trying
data = rnorm(100000)
hist(data, col="grey50")
abline(v=-2,col="red")
plot(density(data))

# Tut exercise
pnorm(2,1,4)
qchisq(0.7,4)

