# uniform -> normal using box muller transformation
u1 = runif(10000)
hist(u1)
mean(u1)
var(u1)

u2 <- runif(10000)


z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
z2 <- sqrt(-2*log(u1)) * cos(2*pi*u2)

layout(matrix(1:2, 1, 2))
hist(z1); hist(z2)
qqnorm(z1); qqnorm(z2)

# for the normal random variable cor = 0 -> indep
cor(z1, z2)


# exp(1)
x <- -log(1-u1)
hist(x);
mean(x); var(x) # almost 1


# second lecture
edu <- read.csv("Introduction to Regression Analysis/datasets/edu_multi.csv")
View(edu)
str(edu)
fit <- lm(ACHV~FAM+PEER+SCHOOL, data = edu)
summary(fit)
# SCHOOL's estiamte does not make sense

layout(matrix(1:4, 2, 2)); plot(fit)

# explanatory vaiable -> positively correlated
plot(edu)

# computing VIF
fit_F <- lm(FAM~PEER+SCHOOL, data = edu); summary(fit_F)

1/(1-0.9734)


# 
install.packages("HH")
library(HH)
vif(fit)


# 
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
compute_vif(edu[, 2:4])

# SCHOOL -> large VIF
fit <- lm(ACHV ~ FAM + PEER, data = edu); summary(fit)

# variable 2개 -> vif 같은 값이겠지
# 둘다 10 이상 -> 하나 버려
compute_vif(edu[, -c(1, 4)])

# r-squared는 별로지만 p-value나 estimate의 sign reasonable
fit <- lm(ACHV ~ FAM, data = edu); summary(fit)


ad <- read.delim("Introduction to Regression Analysis/datasets/advertising.txt")
View(ad)
plot(ad)
summary(ad)
str(ad)

fit <- lm(St~At+Pt+Et+At.1+Pt.1, data = ad); summary(fit)
vif(fit)
vif(ad[,-1])



















