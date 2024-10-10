pacman::p_load(rio, dplyr, magrittr)

dat <- import("data5.xlsx")
freq_table <- as.data.frame(table(dat$x))
names(freq_table) <- c("group", "frequency")
sumx_ <- sum(freq_table$frequency)
meanx_ <- mean(dat$x)

for (i in 1:length(freq_table$frequency)) {
  k <- freq_table$frequency[i]
  poisson_prob <- (exp(-meanx_) * (meanx_^(k / sumx_))) / factorial(k / sumx_)
  print(poisson_prob)
}

freq_table
k <- 1
poisson_prob <- (exp(-meanx_) * (meanx_^(k / sumx_))) / factorial(k / sumx_)
poisson_prob
