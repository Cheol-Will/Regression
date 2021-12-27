mpg <- read.csv("Introduction to Regression Analysis/datasets/MPG.csv", header = T); View(mpg)
names(mpg)[1] <- "MPG"
mpg
summary(mpg)
mpg_fit <- lm(MPG~Weight+Odometer, data = mpg)
summary(mpg_fit)

mpg_fit$residuals
mpg_fit$fitted.values
rstandard(mpg_fit)
qqnorm(rstandard(mpg_fit))
qqline(rstandard(mpg_fit))
plot(mpg_fit$fitted.values, rstandard(mpg_fit))
hatvalues(mpg_fit)
cooks.distance(mpg_fit) > 0.4
plot(mpg_fit)
plot(mpg_fit, which = 5)




bacteria <- read.csv("C:/Rstudy/Introduction to Regression Analysis/datasets/bacteria.csv")
View(bacteria)
plot(bacteria)
result= lm(nt~t, data = bacteria)
# R squared not bad
summary(result)
plot(result)
# there's a parttern in Residual vs Fitted -> not allowed
plot(result, which = 1)


bacteria$lognt <- log(bacteria$nt)
plot(bacteria$t, bacteria$lognt)
result2 <- lm(lognt ~ t, data = bacteria)
# R square so good
summary(result2)
# No Patter n in Residual vs Fitted
# In QQ Plot
plot(result2)
plot(bacteria)


artificial1 <- read.csv("C:/Rstudy/Introduction to Regression Analysis/datasets/artificial1.csv")
View(artificial1)
plot(artificial1)
result <- lm(Y~X, artificial1)
summary(result)
plot(result)
plot(result$fitted.values, rstandard(result))
# using layout see the plots at once
layout(matrix(c(1,2,3,4), 2, 2))
plot(result)

# graphs are so bad -> let's use log to reduce the variance
artificial1$newY = log(artificial1$Y)
result2 <- lm(newY~X, artificial1)
summary(result2)
# R squared is better now 
layout(matrix(1:4, 2, 2))
plot(result2)

data <- read.csv("C:/Rstudy/Introduction to Regression Analysis/datasets/artificial2.csv", header = T)
View(data)
result <- lm(Y~X, data)
summary(result)
layout(matrix(1:4, 2, 2))
plot(result)

data$newY <- log(data$Y)
result2 <- lm(newY~X, data)
plot(result2$fitted.values, rstandard(result2))
layout(matrix(1:4, 2, 2)); plot(result2)























