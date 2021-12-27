# sample variance ---------------------------------
sample_var = function(x){
  x_mean = mean(x); print(x_mean)
  
  x_dev <- x-x_mean; print(x_dev)
  x_dev_squ <- x_dev^2; print(x_dev_squ)
  
  total <- sum(x_dev_squ); print(total)
  
  return (total/(length(x) - 1))
}

# sample covariance ---------------------------------
sample_cov = function(x, y){
  x_mean = mean(x); print(x_mean)
  x_dev <- x-x_mean; print(x_dev)
  
  
  y_mean = mean(y); print(y_mean)
  y_dev <- y-y_mean; print(y_dev)
  
  
  x_y_dev <- x_dev * y_dev; print(x_y_dev)
  total <- sum(x_y_dev);   print(total)
  
  return (total/(length(x) - 1))
}

# Sum of squared somthing -------------------------
sxx = function(x){
  x_mean = mean(x); print(x_mean)
  
  x_dev <- x-x_mean; print(x_dev)
  x_dev_squ <- x_dev^2; print(x_dev_squ)
  
  total <- sum(x_dev_squ)
  
  return(total)
}


# Weight ------------------------------------------
x <- c(1, 2, 4, 3)
y <- c(4, 12, 20, 15)
df <- data.frame(x = x, y = y)
result1 <- lm(y~x, data = df, weights = 1/df$x)
summary(result1)

mean_weight <- function(x, w){
  result <- sum(x*w)/sum(w)
  return(result)
}

mean_weight(df$x, 1/df$x)
mean_weight(df$y, 1/df$x)
weighted.mean(df$x, w = 1/df$x)
weighted.mean(df$y, w = 1/df$x)

sxx_weight <- function(x, w){
  a <- mean_weight(x, w)
  mul <- w*(x-a)^2; print(mul)
  result <- sum(mul)
  return(result)
}
sxy_weight <- function(x, y, w){
  a <- mean_weight(x, w)
  b <- mean_weight(y, w)
  mul <- w*(x-a)*(y-b); print(mul)
  result <- sum(mul)
  return(result)
}
sxx_weight(df$x, w = 1/df$x)
sxy_weight(df$x, df$y, w = 1/df$x)

sxy_weight(df$x, df$y, w = 1/df$x)/sxx_weight(df$x, w = 1/df$x)
mean_weight(y, 1/df$x)
mean_weight(x, 1/df$x)

# y_hat, ei, sigma squared_hat, se of beta1_hat
y_hat <- -0.8276 + 5.431 * df$x; y_hat
e_i <- df$y - y_hat; e_i
sigma_suqared_hat <- sum(e_i^2 * 1/df$x)/2; sigma_suqared_hat
var_beta1 <- sum(e_i^2 * 1/df$x)/2/sxx_weight(df$x, w = 1/df$x)
se_beta1 <- sqrt(sum(e_i^2 * 1/df$x)/2/sxx_weight(df$x, w = 1/df$x)); se_beta1




# beta 구하기
solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%df$y

# 


84/13+1
2*7*6*71/13^2/12
