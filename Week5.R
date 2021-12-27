3+4
exp(1)
pi
log(3)
sqrt(3)
2^10
x = 3

sin(2*pi)
(x>1) + (x>0)

x = c(3:5); y = c(1:3); x; y
x/y
mean(x); sum(y); min(x);

seq(1, 8)
seq(1, 8, 2)
rep(x, 2)

x = matrix(c(1, 2, 3, 4), ncol = 2)
y = matrix(10*c(1, 2, 3, 4), ncol = 2)
x; y

x*y
x %*% y
solve(x)
x%*%solve(x)
t(x)
tr(x)
sum(diag(x))
rownames(x) = c('a', 'b'); colnames(x) = c("A", "B"); x
rbind(x,y)
cbind(x, y)


grade = list(midterm = 35, final = 86, homework = c(19, 18, 20)); grade
total = 0.3*grade$midterm + 0.4*grade$final + 0.3*sum(grade$homework)
total

names = letters[1:3]
midterm = c(90, 99, 98); final = c(97, 89, 95)
mydata = data.frame(Name = names, Midterm = midterm, Final = final)
mydata
mydata$Name

read.csv(file.choose())

x = 3
if (x>0){
  print("Positive")
} else {
  print("Negative")
}

x = 1
while (x<5){
  print(x)
  x = x+1
}

x = 1:100; x
which(x%%7 == 0)
sum(which(x%%7 == 0))

test = function(y){
  x = 0
  for (i in 1:y)
  {
    if(i%%7 == 0) x = x+i
  }
  return (x)
}
test(14)
test

x=c(0.19, 0.15, 0.57, 0.4, 0.7, 0.67, 0.63, 0.47, 0.75, 0.6,
    0.78, 0.81, 0.78, 0.69, 1.3, 1.05, 1.52, 1.06, 1.74, 1.62)
y=c(3.8, 5.9, 14.1, 10.4, 14.6, 14.5, 15.1, 11.9, 15.5, 9.3,
    15.6, 20.8, 14.6, 16.6, 25.6, 20.9, 29.9, 19.6, 31.3, 32.7)
plot(x, y)
lm(y~x)
abline(lm(y~x), col = "blue")
result = lm(y~x)
summary(result)


library(stringr)
data = "7.28 10.5 15
5.63 23 71
5.26 27.5 36
6.58 14.5 113
5.01 30.5 39
6.73 14 97
5.37 21 195
7.28 8.5 8
4.85 26 84
5.08 26.5 25
5.51 15 124
4.75 30 25
6.03 15 75
5.26 22.5 192
5.6 16 139

"

data <- gsub("\n", " ", data)
data <- str_split(data, " ")
data
length(data)
data <- unlist(data)
length(data)
data <- data[-c(46, 47)]
data

mpg <- data[seq(1, 45, 3)]; mpg <- as.numeric(mpg)
weight <- data[seq(2, 45, 3)]; weight <- as.numeric(weight)
odometer <- data[seq(3, 45, 3)]; odometer <- as.numeric(odometer)

ex2 = data.frame(MPG = mpg, Weight = weight, Odometer = odometer); ex2

ANOVA = aov(MPG ~ Weight + Odometer, data = ex2)
summary(ANOVA)

ANOVA_1 = aov(MPG ~ Odometer, data = ex2); ANOVA
summary(ANOVA_1)

ANOVA_2 = aov(MPG ~ Weight, data = ex2); ANOVA
summary(ANOVA_2)

l = lm(MPG ~ Weight + Odometer, data = ex2)
summary(l)

data(iris)
result <- aov(Sepal.Length ~ Species, data =iris)
summary(result)




# Excercise 2.9
0.0358 + 0.0544
0.0902/18
0.203311 + 0.656040*0.45














