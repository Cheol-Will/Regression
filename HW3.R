library(car)
df <- read.table("Introduction to Regression Analysis/datasets/salary-1.txt", header = T);
str(df)

#
str(df)
a <- factor(df$Degree)
levels(a) <- c("Bachelor", "Master", "Doctor")
df$Degree <- a
str(df)
cor(df[,c(1, 3, 4)])

plot(df)
write.table(summary(df), 'Introduction to Regression Analysis/data_summary.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(table(str(df)), 'Introduction to Regression Analysis/data_structure.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)

jpeg(file="Introduction to Regression Analysis/saving_plot1.jpeg")
plot(df)
dev.off()


result1 <- lm(salary~Degree + Years + Num, data = df)
summary(result1) # R^2: 0.8632
layout(matrix(c(1,2,3,4), 2, 2))
jpeg(file="Introduction to Regression Analysis/result1.jpeg")
plot(result1)
dev.off()
vif(result1) # 다중공선성은 10부터이기는 하지만 아직 ㄱㅊ
step(result1, direction = "backward") # 변수 다 써


result1_1 <- lm(salary~Degree + Years + Num, data = df) # influential point 43 지워
summary(result1_1) # R^2: 0.8822 개선 되었다
layout(matrix(c(1,2,3,4), 2, 2))
plot(result1_1)
vif(result1_1) # 다중공선성은 10부터이기는 하지만 간당간당하네
step(result1_1, direction = "backward") # 변수 다 써


df$log_sal <- log(df$salary) 
result2 <- lm(log_sal~Degree+Years+Num, data = df) # influential point 43 지워
summary(result2) # R^2: 0.8493 -> 지우고 난 후 R^2: 0.8624
layout(matrix(c(1,2,3,4), 2, 2))
plot(result2) # R vs F.V not bad, QQ plot better
vif(result2)


compute_vif <- function(x_mat){
  size_x = dim(x_mat)
  vif <- 0*x_mat[1,]; rownames(vif) <- "VIF"
  for(i in 1:size_x[2]){
    y <- as.matrix(x_mat[,i]); x <- as.matrix(x_mat[,-i])
    result <- summary(lm(y~x))
    vif[1, i] <- 1/(1-result$r.squared)
  }
  return(vif)
}
compute_vif(df[,c(1, 3)])




df$sqrt_saSXl <- sqrt(df$salary)
result3 <- lm(sqrt_sal~Degree+Years+Num, data = df) # influential point 43 지워
summary(result3) # R^2: 0.8612 -> 지우고 난 후 R^2: 0.878
layout(matrix(c(1,2,3,4), 2, 2))
plot(result3) # R vs F.V와 QQ plot 둘 다 별로


# year에 sqaure num에 sqrt
df_4 <- df; df_4
df_4$years_sqr <- df_4$Years^2; 
df_4$num_sqrt <- sqrt(df_4$Num); df_4
result4 <- lm(salary~ Degree+years_sqr+num_sqrt, data = df_4)
summary(result4) #R^2: 0.832
layout(matrix(c(1,2,3,4), 2, 2))
plot(result4)


# Standardization
stand <- function(x){
  result <- (x-my_mean(x))/sd(x)
  return(result)
}
df_5 <- df[, 1:4]; df_5
df_5$salary
df_5$stand_sal <- stand(df_5$salary) 
df_5$stand_Years <- stand(df_5$Years)

result5 <- lm(stand_sal~Degree + stand_Years + Num, data = df_5)
summary(result5) # R^2: 0.8633 -> 지우고 난 후 R^2: 0.8822
layout(matrix(c(1,2,3,4), 2, 2))
plot(result5) # F.V vs R bad, QQ bad
vif(result5) # 다중공선성은 10부터이기는 하지만 아직 ㄱㅊ
step(result5, direction = "backward") # 변수 다 써


# standardization years에만 log salary
df_5$log_sal <- log(df_5$salary)
result6 <- lm(log_sal~Degree + stand_Years + Num, data = df_5)
summary(result6) # R^2: 0.8624, original: 0.8493
layout(matrix(c(1,2,3,4), 2, 2))
plot(result6) # F.V vs R good, QQ good
vif(result6) # 다중공선성은 10부터이기는 하지만 아직 ㄱㅊ
step(result6, direction = "backward") # 변수 다 써