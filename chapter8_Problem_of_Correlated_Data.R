data_ <- "1 0.9090 2.200 0.03635 
2 0.08942 2.222 0.03345 
3 0.09755 2.244 0.03870 
4 0.09550 2.267 0.03745 
5 0.09678 2.280 0.04063 
6 0.10327 2.289 0.04237 
7 0.10513 2.289 0.04715 
8 0.10840 2.290 0.04883 
9 0.10822 2.299 0.04836 
10 0.10741 2.300 0.05160 
11 0.10751 2.300 0.04879 
12 0.11429 2.340 0.05523 
13 0.11048 2.386 0.04770 
14 0.11604 2.433 0.05282 
15 0.11688 2.482 0.05473 
16 0.12044 2.532 0.05531 
17 0.12125 2.580 0.05898 
18 0.12080 2.605 0.06267 
19 0.12368 2.631 0.05462 
20 0.12679 2.658 0.05672 
21 0.12996 2.684 0.06674 
22 0.13445 2.711 0.06451 
23 0.13325 2.738 0.06313 
24 0.13863 2.766 0.06573 
25 0.13964 2.793 0.07229 "
data_1<-strsplit(data_, "\n")
data_2 <- strsplit(data_1[[1]], ' ')
data_3 <- unlist(data_2)
data_4 <- as.numeric(data_3)
df <- matrix(data_4, nrow = 25, byrow = T)
df
df <- as.data.frame(df)
df <- df[, 2:4]
colnames(df) <- c("H", "P", "D"); df

library(randtests)
result1 <- lm(H~., data = df); summary(result1)
r = rstandard(result1); r
sign(rstandard(result1))
runs.test(r, "two.sided", threshold = 0) 


# 8.2
data_ <- "
1890 
1900 
1905 
1910 
1915 
1920 
1925 
1930 
1935
30 1940 2,150 1972 18,584 
77 1945 2,595 1974 20,389 
149 1950 3,803 1976 20,188 
215 1955 5,626 1978 21,922 
328 1960 7,674 1980 21,722 
432 1962 8,882 1982 19,411 
689 1964 10.310 1984 19,837 
1,069 1966 12,016 1986 20,246 
1,412 1968 14,104 1988 21,33"
data_1<-strsplit(data_, "\n"); data_1
data_2 <- strsplit(data_1[[1]], ' '); data_2
data_3 <- unlist(data_2); data_3
data_3<-gsub(",", "", x = data_3); data_3
data_4 <- as.numeric(data_3); data_4

df2 <- matrix(data_4, ncol = 2, byrow = T); df2
colnames(df2) <- c("Year", "Oil"); df2
df2 <- as.data.frame(df2); df2
result2 <- lm(Year~., data = df2); summary(result2)
runs.test(result2, "two.sided", threshold = 0) 
r2 = rstandard(result2); r2
sign(rstandard(result2))
runs.test(r2, "two.sided", threshold = 0) 



library(lmtest)
dwtest(df2$Year~df2$Oil)



# 8.3


V I D W G P N

data_ <- "0.5168 1 1 0 2.229 4.252 
0.3612 1 0 1 -11.463 16.535 
0.4176 -1 -1 0 -3.872 5.161 
0.4118 -1 0 0 4.623 0.183 
0.5916 -1 -1 0 -14.901 7.069 
0.6246 1 0 11.921 2.362 
0.5500 0 3.708 0.028 
0.5377 1 4.119 5.678 
0.5237 1 1 1.849 8.722 
0.4460 1 0 0 0.627 2.288 
0.4224 -1 -1 0 -1.527 1.936 
0.5009 -1 0 0 0.114 1.932 
0.6134 1 0 5.054 1.247 
0.4960 0 0 4.836 3.215 
0.3821 -1 -1 0 6.278 4.766 
0.5105 -1 0 0 3.663 7.657 
0.4470 1 0 -3.789 8.093 
0.4083 -1 -1 0 5.387 5.403 
0.4610 -1 0 0 2.068 3.272 
0.5345 -1 -1 0 2.293 3.692 
0.5474 1 1 0 2.918 2.268"
data_1<-strsplit(data_, "\n"); data_1
data_2 <- strsplit(data_1[[1]], ' ')
data_3 <- unlist(data_2)
data_4 <- as.numeric(data_3); data_4
df3 <- matrix(data_4, ncol = 6, byrow = T); df3
df3 <- as.data.frame(df3); df3
# model에 ㅁ자추어야 하는데 ㄹㅇ 개귀찮아
df







