data <- read.csv("C:/Rstudy/Introduction to Regression Analysis/datasets/artificial1.csv", header = T)
View(data)

# original data
result1 <- lm(Y~X, data); summary(result1)

# transfomed data
data$TY <- data$Y/data$X
data$TX <- 1/data$X
result2 <- lm(TY~TX, data); summary(result2)
layout(matrix(1:4, 2, 2)); plot(result2)

# Weighted Least Square (WLS) estimation
result1 <- lm(Y~X, data, weight = 1/X^2); summary(result1)















































