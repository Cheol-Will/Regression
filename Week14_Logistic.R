df <- read.csv("Introduction to Regression Analysis/datasets/admit.csv", header = T); View(df)
str(df)
df[,"rank"] <- factor(df[,"rank"]); str(df)


# Logistic Regression
result <- glm(admit~., data = df, family = "binomial"); summary(result)

pred_y <- ifelse(result$fitted.values >= 0.5, 1, 0)
table(df$admit, pred_y, dnn = c("Observed", "Predicted"))
mean(df$admit == pred_y)


# confusion matrix

#Insatll required packages
install.packages('caret')

#Import required library
library(caret)
confusionMatrix(as.factor(pred_y), as.factor(df$admit))


# Predict
new <- data.frame(gre = 500, gpa = 3.25, rank = as.factor(1))
# link option -> log odds
predict(result, newdata = new, type = "link")
pred <- predict(result, newdata = new, type = "response", se.fit = T)
pred
pred$fit
pred$se.fit

c(pred$fit - 1.96*pred$se.fit, pred$fit + 1.96*pred$se.fit)
