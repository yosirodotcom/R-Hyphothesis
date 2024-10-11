pacman::p_load(rio, dplyr, magrittr, knitr)

# Chi-squared Goodnees of Fit for Nominal Categorical
## Test for Poisson Distribution similarity

### Parameter
alpha <- 0.05
tabel_import <- "data5.xlsx"

dat <- import(tabel_import)
freq_table <- as.data.frame(table(dat$x))
names(freq_table) <- c("group", "frequency")
freq_table$group <- as.numeric(as.character(freq_table$group))
freq_table %<>% mutate(prob = dpois(group, round(mean(dat$x))))
freq_table %<>% mutate(E = prob * sum(frequency))
freq_table_final <- data.frame(frequency = 0, prob = 0, E = 0)

j <- 0
for (i in 1:nrow(freq_table)) {
  less_5 <- freq_table$frequency[i] <= 5
  if (less_5 & i == 1) {
    j <- j + 1
    freq_table_final$frequency[j] <- freq_table$frequency[i]
    freq_table_final$prob[j] <- freq_table$prob[i]
    freq_table_final$E[j] <- freq_table$E[i]
    print(freq_table_final)
  } else if (!less_5 & i == 1) {
    j <- j + 1
    freq_table_final$frequency[j] <- freq_table$frequency[i]
    freq_table_final$prob[j] <- freq_table$prob[i]
    freq_table_final$E[j] <- freq_table$E[i]
    print(freq_table_final)
  } else if (less_5 & i != 1) {
    j <- j + 1

    freq_table_final$frequency[length(freq_table_final$frequency)] <- freq_table_final$frequency[length(freq_table_final$frequency)] + freq_table$frequency[i]
    freq_table_final$prob[length(freq_table_final$frequency)] <- freq_table_final$prob[length(freq_table_final$frequency)] + freq_table$prob[i]
    freq_table_final$E[length(freq_table_final$frequency)] <- freq_table_final$E[length(freq_table_final$frequency)] + freq_table$E[i]
    print(freq_table_final)
  } else {
    j <- j + 1
    temp_df <- data.frame(
      frequency = freq_table$frequency[i],
      prob = freq_table$prob[i],
      E = freq_table$E[i]
    )
    freq_table_final <- rbind(freq_table_final, temp_df)
    print(freq_table_final)
  }
}

freq_table_final

big_table <- freq_table %>% filter(frequency > 5)
small_table <- freq_table %>% filter(frequency <= 5)

small_table <- small_table %>%
  mutate(sum_freq = sum(frequency), sum_prob = sum(prob), sum_E = sum(E)) %>%
  head(1) %>%
  select(-c(frequency, prob, E)) %>%
  rename(frequency = sum_freq, prob = sum_prob, E = sum_E)

freq_table <- rbind(big_table, small_table)
freq_table %<>% mutate(chi = (E - frequency)^2 / E)
db <- length(freq_table$group) - 2
chi_stat <- sum(freq_table$chi)
cv <- qchisq(1 - alpha, df = db)

if (chi_stat > cv) {
  hipotesis <- "H_0 is rejected"
  status <- "Not equal to Poisson Dist."
} else {
  hipotesis <- "H_0 is not rejected"
  status <- "Equal to Poisson Dist."
}

tabel <- data.frame(
  chisquare = chi_stat,
  cv = cv,
  alpha = alpha,
  df = db,
  hipotesis = hipotesis,
  status = status,
  stringsAsFactors = FALSE
)

print(kable(freq_table))
print(kable(tabel))


# Chi-squared Goodnees of Fit for group by interval categorical
## Test for Normal Distribution similarity

### Parameter
alpha <- 0.05
tabel_import <- "data6.xlsx"
mu0 <- 30
var0 <- 100
p <- 4 # Jumlah interval yang diinginkan

dat <- import(tabel_import)
p <- p - 1
prob <- (1:p) / (p + 1)
znorm <- qnorm(prob)

k <- c(min(dat$x) - 0.01, mu0 + sqrt(var0) * znorm, max(dat$x))
freq_table <- dat %>%
  mutate(interval = cut(x, breaks = k, include.lowest = TRUE)) %>%
  count(interval) %>%
  rename(O = n)
freq_table %<>% mutate(E = sum(freq_table$O) / length(freq_table$O)) %>% mutate(chi = (E - O)^2 / E)
db <- length(freq_table$interval) - 1
chi_stat <- sum(freq_table$chi)
cv <- qchisq(1 - alpha, df = db)

if (chi_stat > cv) {
  hipotesis <- "H_0 is rejected"
  status <- "Not equal to Normal Dist."
} else {
  hipotesis <- "H_0 is not rejected"
  status <- "Equal to Normal Dist."
}

tabel <- data.frame(
  chisquare = chi_stat,
  cv = cv,
  alpha = alpha,
  df = db,
  hipotesis = hipotesis,
  status = status,
  stringsAsFactors = FALSE
)

print(kable(freq_table))
print(kable(tabel))
