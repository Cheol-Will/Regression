df <- read.delim("Introduction to Regression Analysis/datasets/advertising.txt")
df
fit1 <- lm(St~., data = df); anova(fit1)
fit2 <- lm(St~. ,data = df[,1:4]); anova(fit2)

anova(fit2, fit1)



# mallocs cp
fit1 <- lm(St~., data = df)
fit2 <- lm(St~., data = df[,1:5])
fit3 <- lm(St~., data = df[,c(1, 3, 4)])

sig <- sum(fit1$residuals^2)/df.residual(fit1)
n <- nrow(df)
sum(fit1$residuals^2)/sig + 2*6 -n
sum(fit2$residuals^2)/sig + 2*4 -n
sum(fit3$residuals^2)/sig + 2*3 -n

AIC(fit1)
AIC(fit2)
AIC(fit3)

BIC(fit1)
BIC(fit2)
BIC(fit3)

# Step
null <- lm(St~1, df)
full <- lm(St~ ., df)

# 각 step마다 변수를 추가하거나 뺄 때 AIC 값을 보고 
# 이전 step의 결과보다 AIC값 작은애가 있으면 그 model을 선택택
step(null, scope = list(lower = null, upper = full), direction = "forward")
step(full, scope = list(lower = null, upper = full), direction = "backward")
step(null, scope = list(lower = null, upper = full), direction = "both")








x <- rnorm(10000, mean = 60, sd = 10)







