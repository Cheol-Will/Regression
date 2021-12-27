# 2 (b)
1 - 409.37/96/75.53

xi <- matrix(c(2, 1, -1), nrow = 1); xi
P <- matrix(c(-0.598, 0.346, 0.723, 
              -0.54, -0.841, -0.044, 
              -0.592, 0.416, -0.69),
            byrow = T, nrow = 3); P

CI <- xi%*%P; CI
alpha_without_0 <- matrix(c(-5.2005, -0.5711, -3.2416), nrow = 3); alpha_without_0
alpha_0 <- 2.0146
y_without_0 <- CI %*% alpha_without_0; y_without_0
y_predict <- y_without_0 + alpha_0; y_predict


beta_without_0 <- P %*% alpha_without_0; beta_without_0
# 5
# a
df5 <- matrix(c(1, 2, 4, 3, 4, 12, 20, 15), ncol = 2); df5
df5 <- as.data.frame(df5); df5
result5 <- lm(V2~V1, df5, weights = (1/V1))
summary(result5)
plot(result5, 1)
df5
x_w <- sum(df5[,1]*(1/df5[,1]))/sum(1/df5[,1]);x_w
y_w <- sum(df5[,2]*(1/df5[,1])/sum(1/df5[,1]));y_w
sxx_w <- sum((df5[,1]-x_w)^2*(1/df5[,1])); sxx_w
sxy_w <- sum((df5[,2]-y_w)*(df5[,1]-x_w)*(1/df5[,1])); sxy_w
beta1_w<- sxy_w/sxx_w; beta1_w
beta0_w <- y_w - beta1_w*x_w; beta0_w

0.7441^2*sxx_w
sse <- sum(1/df5[,1]*(Y - result5$fitted.values)^2); sse
sigma_squ <- sse/2; sigma_squ
sigma_hat <- sqrt(sigma_squ); sigma_hat
se_beta1_w <- sqrt(sigma_hat^2/sxx_w); se_beta1_w


X <- matrix(c(rep(1, 4), df5[,1]), nrow = 4); X
Y <- as.matrix(df5[,2]); Y
W <- diag(1/df5[,1]); W
k <- t(X)%*%W%*%X; k
beta_w <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y; beta_w


se_beta1_w <- sqrt(sigma_hat^2/sxx_w); se_beta1_w

0.7441^2*sxx_w
diag(c(sigma_squ, sigma_squ), ncol = 2)
var_w <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%t(solve(t(X)%*%W%*%X)%*%t(X)%*%W)%*%diag(c(sigma_squ, sigma_squ), ncol = 2)
var_w
sqrt(var_w)


# 6
#(a)
n = 100
p = 3
1-(n-1)/(n-p-1)*(1-0.9)
p = 2
1-(n-1)/(n-p-1)*(1-0.88)          
p = 1
1-(n-1)/(n-p-1)*(1-0.85)

1-0.9
(1-0.88)
(1-0.85)

1*(n-(3+1)) + 8 - 100 # p+1 (Full model)
0.12/0.1*96 + 6 - 100 # 
0.15/0.1*96 + 4 - 100



# (c)
97/2*0.88/0.12


# 7
r <- c(1, 1, -1, 1, 1, -1, -1, 1, -1, 1, -1 , 1, -1)
runs.test(r, "two.sided", threshold = 0) 

r <- c(1.6, 1.5, -2.6, 3.2, 2.6, -2.5, -0.8, 1.9, -3.4, 1.4, -2.2, 2.0, -2.1)

a <- 0
for(i in 2:length(r)){
  a = a+r[i]*r[i-1]
}
b <- sum(r^2)
autocorr <- a/b; autocorr
  
d <- 2*(1-autocorr); d








# dwtest(mydata$nt~mydata$t)