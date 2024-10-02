x1 <- c(0.2,0.25,0.25,0.3,0.4,0.5,0.5)
y1 <- c(30,26,40,35,54,56,65)
x2 <- c(0.2,0.25,0.3,0.4,0.4,0.5)
y2 <- c(23,24,42,49,55,70)


alpha <- 0.05
n1 <- length(x1)
n2 <- length(x2)

sxx1 <- sum(x1^2) - n1*mean(x1)^2
syy1 <- sum(y1^2) - n1*mean(y1)^2
sxy1 <- sum(x1*y1) - n1*mean(x1)*mean(y1)
syy2 <- sum(y2^2) - n2*mean(y2)^2
sxx2 <- sum(x2^2) - n2*mean(x2)^2
sxy2 <- sum(x2*y2) - n2*mean(x2)*mean(y2)

B1 <- sxy1 / sxx1
B2 <- sxy2 / sxx2

a1 <- mean(y1) - B1*mean(x1)
a2 <- mean(y2) - B2*mean(x2)

JKS1 <- syy1 - (sxy1^2) / sxx1
JKS2 <- syy2 - (sxy2^2) / sxx2

spool <- (JKS1 + JKS2) / (n1+n2-4)

sesatan <- sqrt(spool * (1/sxx1 + 1/sxx2))

t_stat_slope <- (B1 - B2) / sesatan
cv_slope <- qt(1-alpha/2, df = n1 + n2 - 4)

Bpool <- (sxy1+sxy2) / (sxx1+sxx2)

t_stat_intercept <- (mean(y1) - mean(y2) - Bpool*(mean(x1) - mean(x2))) / sqrt(spool*(1/n1 + 1/n2 + (mean(x1) - mean(x2)) / (sxx1 + sxx2)))
cv_intercept <- qt(1-alpha/2, df = n1 + n2 - 3)

if (t_stat_slope > cv_slope) {
  print("H0 is rejected")
  print("the slopes are significantly different")
} else {
  print("H0 is not rejected")
  print("the slopes are not significantly different")
}

if (t_stat_intercept > cv_intercept) {
  print("H0 is rejected")
  print("the intercepts are significantly different")
} else {
  print("H0 is not rejected")
  print("the intercepts are not significantly different")
}







x1 <- c(10.4,10.8,11.1,10.2,10.6,11.3,11.6,11.4,10.7,10.9,11.3,11.4,11.5,11.7)
y1 <- c(7.4,7.6,7.9,7.2,8.1,8.5,8.7,8.3,7.5,7.4,7.9,8.9,8.8,9.7)
x2 <- c(10.7,10.5,10.9,11.7,11.2,11.6,11.9,12.1,12.4,12.5,11.4)
y2 <- c(7.9,8.1,8.5,8.9,8.8,9.1,9.4,9.8,9.9,10.1,10.5)
x3 <- c(11.2,11.7,10.5,10.9,10.3,11.5,11.8,11.4,12.1,12.7,12.5,12.3)
y3 <- c(8.1,9.2,9.3,9.8,8.9,9.5,9.7,9.3,9.9,10.3,10.2,10.5)

alpha <- 0.05
n1 <- length(x1)
n2 <- length(x2)
n3 <- length(x3)

sxx1 <- sum(x1^2) - n1*mean(x1)^2
syy1 <- sum(y1^2) - n1*mean(y1)^2
sxy1 <- sum(x1*y1) - n1*mean(x1)*mean(y1)
syy2 <- sum(y2^2) - n2*mean(y2)^2
sxx2 <- sum(x2^2) - n2*mean(x2)^2
sxy2 <- sum(x2*y2) - n2*mean(x2)*mean(y2)
sxx3 <- sum(x3^2) - n3*mean(x3)^2
syy3 <- sum(y3^2) - n3*mean(y3)^2
sxy3 <- sum(x3*y3) - n3*mean(x3)*mean(y3)


B1 <- sxy1 / sxx1
B2 <- sxy2 / sxx2
B3 <- sxy3 / sxx3

Bpool <- (sxy1 + sxy2 + sxy3) / (sxx1 + sxx2 + sxx3)

a1 <- mean(y1) - B1*mean(x1)
a2 <- mean(y2) - B2*mean(x2)
a3 <- mean(y3) - B3*mean(x3)

JKS1 <- syy1 - (sxy1^2) / sxx1
JKS2 <- syy2 - (sxy2^2) / sxx2
JKS3 <- syy3 - (sxy3^2) / sxx3

JKSpool <- (JKS1 + JKS2 + JKS3)
dbpool <- (n1-2)+(n2-2)+(n3-2)

JKSber <- (syy1 + syy2 + syy3) - (sxy1 + sxy2 + sxy3)^2 / (sxx1 + sxx2 + sxx3)
dbber <- (n1+n2+n3-3-1)

xt <- sum(x1)+sum(x2)+sum(x3)
xt_2 <- sum(x1^2)+sum(x2^2)+sum(x3^2)
yt <- sum(y1)+sum(y2)+sum(y3)
yt_2 <- sum(y1^2)+sum(y2^2)+sum(y3^2)
xyt <- sum(x1*y1)+sum(x2*y2)+sum(x3*y3)

sxxt <- xt_2 - (xt^2)/(n1+n2+n3)
syyt <- yt_2 - (yt^2)/(n1+n2+n3)
sxyt <- xyt - (xt*yt)/(n1+n2+n3)

JKSt <- syyt - (sxyt^2) / sxxt
dbt <- (n1+n2+n3-2)

F_slope <- ((JKSber - JKSpool)/(3-1))/(JKSpool/(dbpool))
cv_slope <- qf(1-alpha, 3-1, dbpool)

F_intercept <- ((JKSt - JKSpool)/(2*3-2))/(JKSpool/(dbpool))



