mydata <- read.csv("Introduction to Regression Analysis/datasets/bacteria.csv", header = T); View(mydata)
result_b <- lm(nt~t, data = mydata)
result_b$residuals # 첫 번째는 outlier 오지네
r = rstandard(result_b) # 
sign(rstandard(result_b)) # run test 해야하니까 부호를 보자
# run 개수가 3개네 --> too small!!!
# positive = 5, negative = 10
# n_1 = 5, n_2 = 10
# mean, var 계산할 수 있지?
# test 바로 갈 수 있겠네

# run test library 이용하자
# h0: indep vs h1: dep
install.packages("randtests")
library(randtests)
runs.test(r, "two.sided", threshold = 0) 
runs.test(r, "two.sided", threshold = 1)
# run 나누는 기준을 threshold로 정할 수 있다.
# p-value에 따라 reject h0 -> dependent!!!



# random walk data
rdata <- read.csv(url("http://homepage.stat.uiowa.edu/~kchan/TSA/Datasets/rwalk.dat"), head = T)
rdata$time <- seq(60)
rdata
plot(rdata$time, rdata$rwalk)
plot(rdata$time, rdata$rwalk, type = 'l')
result <- lm(rwalk~time, data = rdata)
summary(result)
layout(matrix(1:4, 2, 2))
plot(result)

rstandard(result)
library(randtests)
runs.test(rstandard(result), "two.sided", threshold = 0)
# 독립이라 말하기 힘들다

rbinom(1, 1, 0.5)
rbinom(100, 1, 0.5)

2*rbinom(100, 1, 0.5) - 1
cumsum(2*rbinom(100, 1, 0.5) - 1)
plot(1:100, cumsum(2*rbinom(100, 1, 0.5) - 1))

plot(1:100, cumsum(2*rbinom(100, 1, 0.5) - 1), type = 'l')
# 이건 진짜 랜덤같다.

# Durbin-Watson Test
install.packages("lmtest")
library(lmtest)
mydata <- read.csv("Introduction to Regression Analysis/datasets/bacteria.csv", header = T);
dwtest(mydata$nt~mydata$t)
# null hypothesis: independence -> result: dependece
dwtest(rdata$rwalk~rdata$time)
# null hypothesis: independence -> result: dependence


# auto correlation
b_result <- lm(nt~t, data = mydata)
r_result <- lm(rwalk~time, data = rdata)

b_res <- b_result$residuals
r_res <- r_result$residuals

acf(b_res)
acf(r_res)
















