# Question 8
# a) 
a = qchisq(0.9,df=4)
a
#     for F = (Z^2/1)/(X/4) ~ F(1,2) -> 4F = (Z^2)/(X) 
qf(0.9,1,4)
b =qf(0.9,1,4)/4
b
#     for T = (Z)/(X/4)^1/2 ~ t(4) -> 2T = (Z)/(X)^1/2
c = qt(0.9,4)/2
c

# b)
x <- a
y <- b
t <- c
# i)
pchisq(x,4)
# ii)
pf(4*y,1,4)
# iii)
pf(4/y,1,4)
# iv)
pt(2*t,4)
# v)
2*(1-pt(t/2,4))

# c)
qt(0.95,4)/2
T1 <-qt(0.95,4)/2
a <- round((T1^2), digits = 4)
b <- round((y), digits = 4)
if (a==b){
  print("the square of  5% upper quantile of Z/√X = y")
} else {
  print("the square of  5% upper quantile of Z/√X != y")
}


