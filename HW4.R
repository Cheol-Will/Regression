df <- read.delim("C:/Rstudy/Introduction to Regression Analysis/datasets/education1970.txt", 
                 header=T)
row.names(df) = df[,1]
df = df[,2:6]
View(df)
str(df)
summary(df)
plot(df)

# factor 1=Northeast, 2=North Central, 3=South, 4=West
# df[df[,5] == 4, 5] = "West"
region <- df[,5]; region
region[region == 1] = "Northeast"
region[region == 2] = "North Central"
region[region == 3] = "South"
region[region == 4] = "West"
region
region_fac <- factor(region, levels = c("Northeast", "North Central", "South", "West"))

df[,5] <- region_fac; df
plot(df)
cor(df[,1:4])
library(HH)
vif(df[2:4])


# (a) without region
result_a <- lm(Y~X1+X2+X3, data = df[,1:4])
summary(result_a)
layout(matrix(1:4, 2, 2))
plot(result_a)


# (c) AK considered as a high LEVERAGE point
# residuals vs leverage plot 보면 ak 가 leverage가 크네


# it changed a lot
summary(result_c)


# (d) no one larger than 2, but NM, AK, CT together make a large changed.
# cook's distance 기반한 potential 이므로 ct는 거론을 해주자.
cooks.distance(result_a)
idx_c <- which(row.names(df) == "NM " | row.names(df) == "AK " | row.names(df) == "CT ")
df_c <- df[-idx_c, 1:4]
result_c <- lm(Y~X1+X2+X3, data = df_c); summary(result_c)

# (e)
df
result_e <- lm(Y~X1+X2+X3+Region, data = df); summary(result_e)
layout(matrix(1:4, 2, 2))
plot(result_e)

# (f)
summary(result_e)

# (g)
# interaction between region and x1...?
# result_g <- glm(Region~X1, data = df)
plot(df[,c(5, 2)])



