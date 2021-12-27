A <- matrix(c(3, 2, 2, 3), 2, 2, byrow = T); A
eigen(A)

eigen_vector <- eigen(A)$vectors
eigen_value <- diag(eigen(A)$values)
eigen_vector %*% eigen_value %*% t(eigen_vector)

library(MASS)
library(mvtnorm)
mean_vector <- c(0, 0)
cov_mat <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
mysample <- rmvnorm(1000, mean_vector, cov_mat)

mean(mysample)
mean(mysample[,1])
mean(mysample[,2])

var(mysample[,1])
var(mysample[,2])
cov(mysample)

# estimate variance covariance matrix
est_cov <- cov(mysample); est_cov

# 아래 식은 평균도 빼지 않는 이상한 식이지만
# 나름 값은 유사하게 나온다
t(mysample) %*% mysample/999


eigen_vector <- eigen(est_cov)$vectors; eigen_vector
eigen_value <- diag(eigen(est_cov)$values); eigen_value
eigen_vector %*% eigen_value %*% t(eigen_vector); est_cov

# multicollinearity
plot(mysample)
abline(v = 0, h = 0)
new_axis1 <- eigen_vector[1, 1]/eigen_vector[2, 1]
new_axis2 <- eigen_vector[1, 2]/eigen_vector[2, 2]
abline(c(0, new_axis1), col = "red")
abline(c(0, new_axis2), col = "red")



cov_mat <- matrix(c(1, 0.99, 0.99, 1), 2, 2)
mysample <- rmvnorm(1000, mean_vector, cov_mat)
est_cov <- cov(mysample); est_cov
eigen_vector <- eigen(est_cov)$vectors; eigen_vector
eigen_value <- diag(eigen(est_cov)$values); eigen_value
eigen_vector %*% eigen_value %*% t(eigen_vector); est_cov

# multicollinearity
plot(mysample)
abline(v = 0, h = 0)
new_axis1 <- eigen_vector[1, 1]/eigen_vector[2, 1]
new_axis2 <- eigen_vector[1, 2]/eigen_vector[2, 2]
abline(c(0, new_axis1), col = "red")
abline(c(0, new_axis2), col = "red")





# 
df <- read.delim("Introduction to Regression Analysis/datasets/education1970.txt")
Y <- df[,2]
X <- df[,3:5]
TX <- scale(X, center = T, scale = T)
TY <- scale(Y, center = T, scale = T)
eig <- eigen(t(TX)%*%TX)
P <- eig$vectors
L <- eig$values; L
C <- TX%*%P
result1 <- lm(TY~C-1)
summary(result1)

df <- read.csv("Introduction to Regression Analysis/datasets/edu_multi.csv")
df
library("MASS")
result <- lm.ridge(ACHV~ ., data = df, lambda = 0.37)
plot(lm.ridge(ACHV~ ., data = df, lambda = seq(0, 30, 0.01)))
select(lm.ridge(ACHV~ ., data = df, lambda = seq(0, 30, 0.01)))
