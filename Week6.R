data()
str(diamonds)

Y = as.vector(diamonds$price[1:100])
X = as.matrix(cbind(1, diamonds$carat[1:100], diamonds$table[1:100]));x

t(X)%*%X
solve(t(X)%*%X)%*%t(X)%*%Y
beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y; beta_hat

H = X%*%solve(t(X) %*% X) %*% t(X); H
Y_Hat = H%*%Y; Y_Hat
X%*%beta_hat

e = Y-Y_Hat;e
sum(e^2)/98
sigma2 = sum(e^2)/98
sigma = sqrt(sigma2); sigma

R_square = cor(Y_Hat, Y)^2; R_square

co = solve(t(X)%*%X)*sigma2; co
sqrt(diag(co))

dia_fit = lm(Y~X)
summary(dia_fit)



# Exercise 3.6 (Table 3.11)
-23.4325/12.74
0.1528 * 8.32
1848.76/1.271296^2
(0.1528^2 * 1143.898) ^ (1/2)
5.167935^2 * 18
480.7359/18
1848.76/(1848.76 + 480.7359)
1848.76/26.70755


# Exercise 3.7 (Table 3.12)
7.342^2 * 16
862.4794/16
862.4794/0.284
3036.899 - 852.4794
2184.42/53.90496

# Exercise 3.14 (Table 3.15)
7862535 + 38460756
38460756/46323291
 








