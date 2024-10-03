pacman::p_load(magrittr, dplyr, ggplot2)

dat1 <- readxl::read_xlsx("data1.xlsx")
dat2 <- readxl::read_xlsx("data2.xlsx")
dat3 <- readxl::read_xlsx("data3.xlsx")

intercept_test <- function(dat, alpha) {
  # list all group
  group <- unique(dat$group)

  x <- list()  
  y <- list()
  meanX <- list()
  meanY <- list()
  n <- list()
  Sxx <- list()
  Syy <- list()
  Sxy <- list()
  B <- list()
  a <- list()
  JKS <- list()
  for (i in group){
    x[[i]] <- dat %>% filter(group == i) %>% pull(x)
    y[[i]] <- dat %>% filter(group == i) %>% pull(y)
    n[[i]] <- length(x[[i]])    
    Sxx[[i]] <- sum(x[[i]]^2) - n[[i]]*mean(x[[i]])^2
    Syy[[i]] <- sum(y[[i]]^2) - n[[i]]*mean(y[[i]])^2
    Sxy[[i]] <- sum(x[[i]]*y[[i]]) - n[[i]]*mean(x[[i]])*mean(y[[i]])
    B[[i]] <- Sxy[[i]] / Sxx[[i]]
    a[[i]] <- mean(y[[i]]) - B[[i]]*mean(x[[i]])
    JKS[[i]] <- Syy[[i]] - (Sxy[[i]]^2)/Sxx[[i]]
    meanX[[i]] <- mean(x[[i]])
    meanY[[i]] <- mean(y[[i]])
  }
  Sgab <- sum(unlist(JKS))/(sum(unlist(n)) - 4)
  sesatan <- sqrt(Sgab * (1/Sxx[[1]] + 1/Sxx[[2]]))
  t_stat_slope <- (B[[1]] - B[[2]]) / sesatan
  cv_slope_l <- qt(alpha/2, df = sum(unlist(n)) - 4)
  cv_slope_r <- qt(1-alpha/2, df = sum(unlist(n)) - 4)  

  Bpool <- sum(unlist(Sxy)) / (sum(unlist(Sxx)))

  t_stat_intercept <- (meanY[[1]]-meanY[[2]]-Bpool*(meanX[[1]]-meanX[[2]]))/(sqrt(Sgab)*sqrt((1/n[[1]]+1/n[[2]]+((meanX[[1]]-meanX[[2]])/(Sxx[[1]]+Sxx[[2]])))))
  cv_intercept_l <- qt(alpha/2, df = sum(unlist(n)) - 3)
  cv_intercept_r <- qt(1-alpha/2, df = sum(unlist(n)) - 3)



  if (abs(t_stat_slope) > abs(cv_slope_l)) {
    slope <- "Tidak Paralel"
    if (abs(t_stat_slope) > abs(cv_slope_r)) {
      intercept <- "Tidak Berimpit"
      text_content <- paste0("H0:B1=B2 ditolak dan H0:a1=a2 ditolak, Garis regresi populasinya tidak sejajar dan tetapi tidak berimpit")
    } else {
      intercept <- "Berimpit"
      text_content <- paste0("H0:B1=B2 ditolak dan H0:a1=a2 tidak ditolak, Garis regresi populasinya tidak sejajar, tetapi berimpit")
    }    
    text_content <- paste0("Slopes are paralel, with t-stat = ", round(t_stat_slope, 3), " outside critical regions = {t-stat<", round(cv_slope_l, 3), " or t-stat>", round(cv_slope_r, 3),"}")
  } else {
    slope <- "Paralel"
    if (abs(t_stat_intercept) > abs(cv_intercept_l)) {
      intercept <- "Tidak Berimpit"
      text_content <- paste0("H0:B1=B2 tidak ditolak dan H0:a1=a2 ditolak, Garis regresi populasinya sejajar , tetapi tidak berimpit")
    } else {
      intercept <- "Berimpit"
      text_content <- paste0("H0:B1=B2 tidak ditolak dan H0:a1=a2 tidak ditolak, Garis regresi populasinya sejajar dan berimpit")
    }
  }
  p <- ggplot2::ggplot(dat, aes(x = x, y = y, color = group)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", se = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Comparison of Regression Lines")
    
  
  print(text_content)
  tbl_kesimpulan_2grup <- data.frame(
    Coef_a=as.character(round(unlist(a),3)),
    Coef_B=as.character(round(unlist(B),3)),
    SSE=as.character(round(unlist(JKS),3)),      
    stringsAsFactors = FALSE
  )
  rownames(tbl_kesimpulan_2grup) <- c(group)
  tbl_kesimpulan <- data.frame(    
    t_value_slope=c(abs(round(t_stat_slope,3)),""),
    abs_cv_slope=c(round(cv_slope_r,3),slope), 
    t_value_intercept=c(abs(round(t_stat_intercept,3)),""),        
    abs_cv_intercept=c(round(cv_intercept_r,3),intercept),    
    stringsAsFactors = FALSE
  )
  rownames(tbl_kesimpulan) <- c(" Test", "Result")
  print(tbl_kesimpulan_2grup)  


}
intercept_test(dat = dat3, alpha=.05)

x1 <- dat
y1 <- c(30,26,40,35,54,56,65)
x2 <- c(0.2,0.25,0.3,0.4,0.4,0.5)
y2 <- c(23,24,42,49,55,70)


# alpha <- 0.05
# n1 <- length(x1)
# n2 <- length(x2)

# sxx1 <- sum(x1^2) - n1*mean(x1)^2
# syy1 <- sum(y1^2) - n1*mean(y1)^2
# sxy1 <- sum(x1*y1) - n1*mean(x1)*mean(y1)
# syy2 <- sum(y2^2) - n2*mean(y2)^2
# sxx2 <- sum(x2^2) - n2*mean(x2)^2
# sxy2 <- sum(x2*y2) - n2*mean(x2)*mean(y2)

# B1 <- sxy1 / sxx1
# B2 <- sxy2 / sxx2



# a1 <- mean(y1) - B1*mean(x1)
# a2 <- mean(y2) - B2*mean(x2)

# JKS1 <- syy1 - (sxy1^2) / sxx1
# JKS2 <- syy2 - (sxy2^2) / sxx2

# spool <- (JKS1 + JKS2) / (n1+n2-4)

# sesatan <- sqrt(spool * (1/sxx1 + 1/sxx2))

# t_stat_slope <- (B1 - B2) / sesatan
# cv_slope <- qt(1-alpha/2, df = n1 + n2 - 4)

# Bpool <- (sxy1+sxy2) / (sxx1+sxx2)

# t_stat_intercept <- (mean(y1) - mean(y2) - Bpool*(mean(x1) - mean(x2))) / sqrt(spool*(1/n1 + 1/n2 + (mean(x1) - mean(x2)) / (sxx1 + sxx2)))
# cv_intercept <- qt(1-alpha/2, df = n1 + n2 - 3)

# if (t_stat_slope > cv_slope) {
#   print("H0 is rejected")
#   print("the slopes are significantly different")
# } else {
#   print("H0 is not rejected")
#   print("the slopes are not significantly different")
# }

# if (t_stat_intercept > cv_intercept) {
#   print("H0 is rejected")
#   print("the intercepts are significantly different")
# } else {
#   print("H0 is not rejected")
#   print("the intercepts are not significantly different")
# }







# x1 <- c(10.4,10.8,11.1,10.2,10.6,11.3,11.6,11.4,10.7,10.9,11.3,11.4,11.5,11.7)
# y1 <- c(7.4,7.6,7.9,7.2,8.1,8.5,8.7,8.3,7.5,7.4,7.9,8.9,8.8,9.7)
# x2 <- c(10.7,10.5,10.9,11.7,11.2,11.6,11.9,12.1,12.4,12.5,11.4)
# y2 <- c(7.9,8.1,8.5,8.9,8.8,9.1,9.4,9.8,9.9,10.1,10.5)
# x3 <- c(11.2,11.7,10.5,10.9,10.3,11.5,11.8,11.4,12.1,12.7,12.5,12.3)
# y3 <- c(8.1,9.2,9.3,9.8,8.9,9.5,9.7,9.3,9.9,10.3,10.2,10.5)

# alpha <- 0.05
# n1 <- length(x1)
# n2 <- length(x2)
# n3 <- length(x3)

# sxx1 <- sum(x1^2) - n1*mean(x1)^2
# syy1 <- sum(y1^2) - n1*mean(y1)^2
# sxy1 <- sum(x1*y1) - n1*mean(x1)*mean(y1)
# syy2 <- sum(y2^2) - n2*mean(y2)^2
# sxx2 <- sum(x2^2) - n2*mean(x2)^2
# sxy2 <- sum(x2*y2) - n2*mean(x2)*mean(y2)
# sxx3 <- sum(x3^2) - n3*mean(x3)^2
# syy3 <- sum(y3^2) - n3*mean(y3)^2
# sxy3 <- sum(x3*y3) - n3*mean(x3)*mean(y3)


# B1 <- sxy1 / sxx1
# B2 <- sxy2 / sxx2
# B3 <- sxy3 / sxx3

# Bpool <- (sxy1 + sxy2 + sxy3) / (sxx1 + sxx2 + sxx3)

# a1 <- mean(y1) - B1*mean(x1)
# a2 <- mean(y2) - B2*mean(x2)
# a3 <- mean(y3) - B3*mean(x3)

# JKS1 <- syy1 - (sxy1^2) / sxx1
# JKS2 <- syy2 - (sxy2^2) / sxx2
# JKS3 <- syy3 - (sxy3^2) / sxx3

# JKSpool <- (JKS1 + JKS2 + JKS3)
# dbpool <- (n1-2)+(n2-2)+(n3-2)

# JKSber <- (syy1 + syy2 + syy3) - (sxy1 + sxy2 + sxy3)^2 / (sxx1 + sxx2 + sxx3)
# dbber <- (n1+n2+n3-3-1)

# xt <- sum(x1)+sum(x2)+sum(x3)
# xt_2 <- sum(x1^2)+sum(x2^2)+sum(x3^2)
# yt <- sum(y1)+sum(y2)+sum(y3)
# yt_2 <- sum(y1^2)+sum(y2^2)+sum(y3^2)
# xyt <- sum(x1*y1)+sum(x2*y2)+sum(x3*y3)

# sxxt <- xt_2 - (xt^2)/(n1+n2+n3)
# syyt <- yt_2 - (yt^2)/(n1+n2+n3)
# sxyt <- xyt - (xt*yt)/(n1+n2+n3)

# JKSt <- syyt - (sxyt^2) / sxxt
# dbt <- (n1+n2+n3-2)

# F_slope <- ((JKSber - JKSpool)/(3-1))/(JKSpool/(dbpool))
# cv_slope <- qf(1-alpha, 3-1, dbpool)

# F_intercept <- ((JKSt - JKSpool)/(2*3-2))/(JKSpool/(dbpool))








# x1 <- c(10.4,10.8,11.1,10.2,10.6,11.3,11.6,11.4,10.7,10.9,11.3,11.4,11.5,11.7)
# y1 <- c(7.4,7.6,7.9,7.2,8.1,8.5,8.7,8.3,7.5,7.4,7.9,8.9,8.8,9.7)
# x2 <- c(10.7,10.5,10.9,11.7,11.2,11.6,11.9,12.1,12.4,12.5,11.4)
# y2 <- c(7.9,8.1,8.5,8.9,8.8,9.1,9.4,9.8,9.9,10.1,10.5)
# x3 <- c(11.2,11.7,10.5,10.9,10.3,11.5,11.8,11.4,12.1,12.7,12.5,12.3)
# y3 <- c(8.1,9.2,9.3,9.8,8.9,9.5,9.7,9.3,9.9,10.3,10.2,10.5)


# data3<- data.frame(
#   x = c(x1, x2, x3),
#   y = c(y1, y2, y3),
#   group = factor(c(rep("A", length(x1)), rep("B", length(x2)), rep("C", length(x3))))
# )

# # export to excel
# pacman::p_load(writexl, multcomp)
# write_xlsx(data3, "data3.xlsx")



# library(car)

# # Fit the model
# model <- lm(y ~ x + group, data = data3)

# # Calculate the F-statistic for the slope
# F_slope2 <- anova(model)$F[1]

# # Calculate the F-statistic for the intercept
# F_intercept2 <- anova(model)$F[2]


# model_intercept <- lm(y ~ x + group + x:group, data = data3)

# # Melihat ringkasan model
# summary(model_intercept)

# # Melakukan uji t untuk kesamaan intersep
# library(multcomp)
# contrast <- glht(model_intercept, linfct = c("groupGroup2 = 0"))
# summary(contrast)

# anova_result <- anova(model_intercept)
# print(anova_result)


# library(emmeans)
# emm <- emmeans(model_intercept, ~ group)
# pairs(emm)