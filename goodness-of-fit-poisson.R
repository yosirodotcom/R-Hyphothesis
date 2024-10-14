pacman::p_load(rio, dplyr, magrittr, knitr)

# Chi-squared Goodnees of Fit for Nominal Categorical
## Test for Poisson Distribution similarity

fit_poisson <- function(tabel_import, alpha) {
  dat <- import(tabel_import)
  freq_table <- as.data.frame(table(dat$x))
  names(freq_table) <- c("group", "frequency")
  freq_table$group <- as.numeric(as.character(freq_table$group))
  freq_table %<>% mutate(prob = dpois(group, round(mean(dat$x))))
  freq_table %<>% mutate(E = prob * sum(frequency))

  iter_first <- TRUE
  less_5_group <- 0
  min_freq <- 5
  # Finding frequency < 5 in the first row
  while (iter_first == TRUE) {
    if (freq_table$frequency[1] < min_freq & freq_table$frequency[2] >= min_freq) { # if frequency < 5 in the first row
      freq_table$frequency[2] <- freq_table$frequency[1] + freq_table$frequency[2]
      freq_table$prob[2] <- freq_table$prob[1] + freq_table$prob[2]
      freq_table$E[2] <- freq_table$E[1] + freq_table$E[2]
      freq_table <- freq_table[-1, ]
      less_5_group <- less_5_group + 1
    } else if (freq_table$frequency[1] < min_freq & freq_table$frequency[2] < min_freq) {
      freq_table$frequency[1] <- freq_table$frequency[1] + freq_table$frequency[2]
      freq_table$prob[1] <- freq_table$prob[1] + freq_table$prob[2]
      freq_table$E[1] <- freq_table$E[1] + freq_table$E[2]
      freq_table <- freq_table[-2, ]
    } else {
      iter_first <- FALSE
    }
  }

  # Finding frequency < 5 in the second row and so on

  index_iter <- 2
  iter_second <- TRUE
  while (iter_second) {
    logic1 <- (index_iter + 1 != length(freq_table$frequency)) & (freq_table$frequency[index_iter] <= min_freq & freq_table$frequency[index_iter + 1] <= min_freq)
    logic2 <- index_iter + 1 == length(freq_table$frequency) & freq_table$frequency[index_iter + 1] <= min_freq
    logic3 <- index_iter + 1 == length(freq_table$frequency) & freq_table$frequency[index_iter + 1] > min_freq

    if (logic2 == TRUE) {
      freq_table$frequency[index_iter] <- freq_table$frequency[index_iter] + freq_table$frequency[index_iter + 1]
      freq_table$prob[index_iter] <- freq_table$prob[index_iter] + freq_table$prob[index_iter + 1]
      freq_table$E[index_iter] <- freq_table$E[index_iter] + freq_table$E[index_iter + 1]
      freq_table <- freq_table[-(index_iter + 1), ]
      less_5_group <- less_5_group + 1
      break
    } else if (logic3 == TRUE) {
      break
    }

    searching <- 0

    if (index_iter < length(freq_table$frequency)) {
      while (logic1 == TRUE) {
        freq_table$frequency[index_iter] <- freq_table$frequency[index_iter] + freq_table$frequency[index_iter + 1]
        freq_table$prob[index_iter] <- freq_table$prob[index_iter] + freq_table$prob[index_iter + 1]
        freq_table$E[index_iter] <- freq_table$E[index_iter] + freq_table$E[index_iter + 1]
        freq_table <- freq_table[-(index_iter + 1), ]
        searching <- searching + 1
        if (index_iter == length(freq_table$frequency)) {
          break
        }
      }
    } else {
      iter_second <- FALSE
    }


    if (searching > 0) {
      less_5_group <- less_5_group + 1
      searching <- 0
    }
    index_iter <- index_iter + 1
  }


  freq_table %<>% mutate(chi = (E - frequency)^2 / E)
  db <- length(freq_table$group) - 1 - less_5_group
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
}


fit_poisson(tabel_import = "data5.xlsx", alpha = 0.05)
