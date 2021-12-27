df <- read.delim("Introduction to Regression Analysis/datasets/advertising.txt"); df
X <- df[,2:6]
Y <- df[,1]
X1 <- scale(X)
Y1 <- scale(Y)
# correlation
t(X1)%*%X1/21
cor(X)


eg <- eigen(t(X1)%*%X1); eg
lam <- eg$values
p <- eg$vectors

C <- X1%*%p

alp <- solve(t(C)%*%C)%*%t(C)%*%Y1
alp
theta <- p%*%alp; theta
solve(t(X1)%*%X1)%*%t(X1)%*%Y1

# 10.2
lam <- diag(c(1.93, 1.06, 0.01))
P <- matrix(c(0.5, -0.697, 0.514, 0.484, 0.717, 0.501, 0.718, 0.002, -0.696), byrow = T, ncol = 3); P
P%*%lam%*%t(P)
al <- matrix(c(0.67, -0.02, -0.56), ncol = 1); al
theta <- P%*%al; theta
Xtilt <- 