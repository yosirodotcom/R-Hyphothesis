pacman::p_load(rio, dplyr, magrittr, knitr, stringr)

#################################################################
# CHI-SQUARED GOODNEES OF FIT FOR GROUP BY INTERVAL CATEGORICAL #
#            TEST FOR NORMAL DISTRIBUTION SIMILARITY            #
#################################################################


# ~$@<=========== CHARACTERISTICS ===========>@$~#
# ~~>> Diketahui mean dan variansi populasi
# ~~>> Data masih bersifat raw, belum di klasifikasikan berdasarkan interval
# ~~>> Peluang harapan sel dianggap sama
# ~~>> Data dengan frekuensi yang kecil tidak perlu digabungkan
# ~$@<======================================>@$~#

fit_normal_A <- function(tabel_import, mu0, var0, p, alpha) {
  # ~$@<<--------- Split data into interval ---------->>@$~#
  dat <- import(tabel_import)
  p <- p - 1
  prob <- (1:p) / (p + 1)
  znorm <- qnorm(prob)
  k <- c(min(dat$x) - 0.01, mu0 + sqrt(var0) * znorm, max(dat$x))
  freq_table <- dat %>%
    mutate(interval = cut(x, breaks = k, include.lowest = TRUE, right = FALSE, )) %>%
    count(interval) %>%
    rename(O = n)
  freq_table %<>% mutate(E = sum(freq_table$O) / length(freq_table$O)) %>%
    mutate(chi = (E - O)^2 / E)

  db <- length(freq_table$interval) - 1

  # ~$@<<--------- Create table summary ---------->>@$~#
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
}





# ~$@<=========== CHARACTERISTICS ===========>@$~#
# ~~>> Tidak diketahui mean dan variansi populasi
# ~~>> Data diklasifikasikan berdasarkan interval yang ditentukan sendiri
# ~~>> Peluang harapan sel berdasarkan peluang normal
# ~~>> Data dengan frekuensi yang kecil digabungkan
# ~$@<======================================>@$~#


fit_normal_B <- function(tabel_import, start_cut, sequence_by, finish_cut, min_merge, alpha) {
  dat <- import(tabel_import)

  freq_table <- dat %>%
    mutate(interval = cut(x,
      breaks = seq(from = start_cut, to = finish_cut, by = sequence_by),
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0(
        "[",
        round(seq(from = start_cut, to = finish_cut, by = sequence_by)[-length(seq(from = start_cut, to = finish_cut, by = sequence_by))], digits = 0),
        ", ",
        round(seq(from = start_cut, to = finish_cut, by = sequence_by)[-1], digits = 0),
        ")"
      )
    )) %>%
    group_by(interval) %>%
    summarise(O = n(), prob = 0, midpoint = 0)

  for (i in 1:length(freq_table$interval)) {
    midpoint <- mean(c(start_cut + (i - 1) * sequence_by, start_cut + (i) * sequence_by))
    freq_table$midpoint[i] <- midpoint
  }
  freq_table %<>% mutate(summid = midpoint * O)
  mu0 <- sum(freq_table$summid) / sum(freq_table$O)
  freq_table %<>% mutate(vari = O * (midpoint - mu0)^2)
  var0 <- sum(freq_table$vari) / (sum(freq_table$O) - 1)
  freq_table %<>% select(interval, O, prob)

  peluang <- 0
  for (i in 1:length(freq_table$interval)) {
    z <- (start_cut + sequence_by * i - mu0) / sqrt(var0)

    freq_table$prob[i] <- pnorm(z) - peluang
    peluang <- pnorm(z)
  }

  freq_table %<>% mutate(E = prob * sum(freq_table$O))

  # ~$@<<--------- MERGE MIN FREQUENCY ---------->>@$~#

  iter_first <- TRUE
  less_5_group <- 0

  # Finding O < 4 in the first row
  while (iter_first == TRUE) {
    if (freq_table$O[1] < min_freq & freq_table$O[2] >= min_freq) { # if O < 5 in the first row
      freq_table$O[2] <- freq_table$O[1] + freq_table$O[2]
      freq_table$prob[2] <- freq_table$prob[1] + freq_table$prob[2]
      freq_table$E[2] <- freq_table$E[1] + freq_table$E[2]
      freq_table <- freq_table[-1, ]
      less_5_group <- less_5_group + 1
    } else if (freq_table$O[1] < min_freq & freq_table$O[2] < min_freq) {
      freq_table$O[1] <- freq_table$O[1] + freq_table$O[2]
      freq_table$prob[1] <- freq_table$prob[1] + freq_table$prob[2]
      freq_table$E[1] <- freq_table$E[1] + freq_table$E[2]
      freq_table <- freq_table[-2, ]
    } else {
      iter_first <- FALSE
    }
  }

  # Finding O < 5 in the second row and so on

  index_iter <- 2
  iter_second <- TRUE
  while (iter_second) {
    logic1 <- (index_iter + 1 != length(freq_table$O)) & (freq_table$O[index_iter] <= min_freq & freq_table$O[index_iter + 1] <= min_freq)
    logic2 <- index_iter + 1 == length(freq_table$O) & freq_table$O[index_iter + 1] <= min_freq
    logic3 <- index_iter + 1 == length(freq_table$O) & freq_table$O[index_iter + 1] > min_freq

    if (logic2 == TRUE) {
      freq_table$O[index_iter] <- freq_table$O[index_iter] + freq_table$O[index_iter + 1]
      freq_table$prob[index_iter] <- freq_table$prob[index_iter] + freq_table$prob[index_iter + 1]
      freq_table$E[index_iter] <- freq_table$E[index_iter] + freq_table$E[index_iter + 1]
      freq_table <- freq_table[-(index_iter + 1), ]
      less_5_group <- less_5_group + 1
      break
    } else if (logic3 == TRUE) {
      break
    }

    searching <- 0

    if (index_iter < length(freq_table$O)) {
      while (logic1 == TRUE) {
        freq_table$O[index_iter] <- freq_table$O[index_iter] + freq_table$O[index_iter + 1]
        freq_table$prob[index_iter] <- freq_table$prob[index_iter] + freq_table$prob[index_iter + 1]
        freq_table$E[index_iter] <- freq_table$E[index_iter] + freq_table$E[index_iter + 1]
        freq_table <- freq_table[-(index_iter + 1), ]
        searching <- searching + 1
        if (index_iter == length(freq_table$O)) {
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

  # ~$@<<--------- CREATE TABLE SUMMARY ---------->>@$~#

  freq_table %<>% mutate(chi = (E - O)^2 / E)
  db <- length(freq_table$interval) - 1 - less_5_group
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
}


# ~$@<<--------- PARAMETER ---------->>@$~#
alpha <- 0.05
tabel_import <- "data6.xlsx"
mu0 <- 30 # if population mean known
var0 <- 100 # if population variance known
p <- 4 # Jumlah interval yang diinginkan

fit_normal_A(tabel_import = tabel_import, mu0 = mu0, var0 = var0, p = p, alpha = alpha)

# ~$@<<--------- PARAMETER ---------->>@$~#
alpha <- 0.05
tabel_import <- "data7.xlsx"
start_cut <- 500
sequence_by <- 500
finish_cut <- 4500
min_freq <- 4

fit_normal_B(tabel_import = tabel_import, start_cut = start_cut, sequence_by = sequence_by, finish_cut = finish_cut, min_merge = min_merge, alpha = alpha)


# ~$@<<--------- PARAMETER ---------->>@$~#
alpha <- 0.05
tabel_import <- "data8.xlsx"
mu0 <- 110 # if population mean known
var0 <- 100 # if population variance known
p <- 4 # Jumlah interval yang diinginkan

fit_normal_A(tabel_import = tabel_import, mu0 = mu0, var0 = var0, p = p, alpha = alpha)


# ~$@<<--------- PARAMETER ---------->>@$~#
alpha <- 0.05
tabel_import <- "data9.xlsx"
start_cut <- 100
sequence_by <- 10
finish_cut <- 200
min_freq <- 5
fit_normal_B(tabel_import = tabel_import, start_cut = start_cut, sequence_by = sequence_by, finish_cut = finish_cut, min_merge = min_merge, alpha = alpha)
