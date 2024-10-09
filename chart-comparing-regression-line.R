pacman::p_load(magrittr, dplyr, ggplot2, readxl, gt, htmltools)

dat0 <- readxl::read_xlsx("data0.xlsx")
dat1 <- readxl::read_xlsx("data1.xlsx")
dat2 <- readxl::read_xlsx("data2.xlsx")
dat3 <- readxl::read_xlsx("data3.xlsx")

slope_test <- function(dat, alpha) {
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
  for (i in group) {
    x[[i]] <- dat %>%
      filter(group == i) %>%
      pull(x)
    y[[i]] <- dat %>%
      filter(group == i) %>%
      pull(y)
    n[[i]] <- length(x[[i]])
    Sxx[[i]] <- sum(x[[i]]^2) - n[[i]] * mean(x[[i]])^2
    Syy[[i]] <- sum(y[[i]]^2) - n[[i]] * mean(y[[i]])^2
    Sxy[[i]] <- sum(x[[i]] * y[[i]]) - n[[i]] * mean(x[[i]]) * mean(y[[i]])
    B[[i]] <- Sxy[[i]] / Sxx[[i]]
    a[[i]] <- mean(y[[i]]) - B[[i]] * mean(x[[i]])
    JKS[[i]] <- Syy[[i]] - (Sxy[[i]]^2) / Sxx[[i]]
    meanX[[i]] <- mean(x[[i]])
    meanY[[i]] <- mean(y[[i]])
  }
  if (length(group) == 2) {
    Sgab <- sum(unlist(JKS)) / (sum(unlist(n)) - 4)
    sesatan <- sqrt(Sgab * (1 / Sxx[[1]] + 1 / Sxx[[2]]))
    t_stat_slope <- (B[[1]] - B[[2]]) / sesatan
    cv_slope_l <- qt(alpha / 2, df = sum(unlist(n)) - 4)
    cv_slope_r <- qt(1 - alpha / 2, df = sum(unlist(n)) - 4)

    Bpool <- sum(unlist(Sxy)) / (sum(unlist(Sxx)))

    t_stat_intercept <- (meanY[[1]] - meanY[[2]] - Bpool * (meanX[[1]] - meanX[[2]])) / (sqrt(Sgab) * sqrt((1 / n[[1]] + 1 / n[[2]] + ((meanX[[1]] - meanX[[2]]) / (Sxx[[1]] + Sxx[[2]])))))
    cv_intercept_l <- qt(alpha / 2, df = sum(unlist(n)) - 3)
    cv_intercept_r <- qt(1 - alpha / 2, df = sum(unlist(n)) - 3)

    if (abs(t_stat_slope) > abs(cv_slope_l)) {
      slope <- "Tidak Paralel"
      if (abs(t_stat_intercept) > abs(cv_intercept_l)) {
        intercept <- "Tidak Berimpit"
        text_content <- paste0("H0:B1=B2 ditolak dan H0:a1=a2 ditolak, Garis regresi populasinya tidak sejajar dan tetapi tidak berimpit")
      } else {
        intercept <- "Berimpit"
        text_content <- paste0("H0:B1=B2 ditolak dan H0:a1=a2 tidak ditolak, Garis regresi populasinya tidak sejajar, tetapi berimpit")
      }
      text_content <- paste0("Slopes are paralel, with t-stat = ", round(t_stat_slope, 3), " outside critical regions = {t-stat<", round(cv_slope_l, 3), " or t-stat>", round(cv_slope_r, 3), "}")
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

    print(text_content)
    tbl_kesimpulan_2grup <- data.frame(
      Coef_a = as.character(round(unlist(a), 3)),
      Coef_B = as.character(round(unlist(B), 3)),
      SSE = as.character(round(unlist(JKS), 3)),
      stringsAsFactors = FALSE
    )
    rownames(tbl_kesimpulan_2grup) <- c(group)
    tbl_kesimpulan <- data.frame(
      t_stat_slope = c(abs(round(t_stat_slope, 3)), ""),
      cv_slope = c(round(cv_slope_r, 3), ""),
      df_slope = c(sum(unlist(n)) - 4, slope),
      t_stat_intercept = c(abs(round(t_stat_intercept, 3)), ""),
      cv_intercept = c(round(cv_intercept_r, 3), ""),
      df_intercept = c(sum(unlist(n)) - 3, intercept),
      stringsAsFactors = FALSE
    )
    rownames(tbl_kesimpulan) <- c(" Test", "Result")

    # Create the first table
    table1 <- tbl_kesimpulan %>%
      gt() %>%
      tab_header(title = "Hypothesis Testing for Regression Lines") %>%
      cols_align(align = "right", columns = everything())

    # Create the second table
    table2 <- tbl_kesimpulan_2grup %>%
      gt() %>%
      tab_header(title = "Coefficients of Regression Lines") %>%
      cols_align(align = "right", columns = everything())
    # Combine the two tables into a single HTML document
    html_doc <- tagList(
      table1,
      table2,
      text_content
    )

    # Display the HTML document in an HTML widget
    html_print(html_doc)
  } else if (length(group) >= 3) {
    JKS_gab <- sum(unlist(JKS))
    db_gab <- sum(unlist(n)) - length(group) * 2
    JKS_pool <- (sum(unlist(Syy))) - (sum(unlist(Sxy)))^2 / (sum(unlist(Sxx)))
    db_pool <- sum(unlist(n)) - length(group) - 1
    Sxxt <- sum(unlist(x)^2) - sum(unlist(x))^2 / sum(unlist(n))
    Syyt <- sum(unlist(y)^2) - sum(unlist(y))^2 / sum(unlist(n))
    Sxyt <- sum(unlist(x) * unlist(y)) - sum(unlist(x)) * sum(unlist(y)) / sum(unlist(n))
    JKS_t <- Syyt - (Sxyt^2) / Sxxt
    db_t <- sum(unlist(n)) - 2
    F_slope <- ((JKS_pool - JKS_gab) / (length(group) - 1)) / (JKS_gab / db_gab)
    cv_slope <- qf(1 - alpha, length(group) - 1, db_gab)
    B_pool <- (sum(unlist(Sxy))) / (sum(unlist(Sxx)))
    F_intercept <- ((JKS_t - JKS_pool) / (length(group) - 1)) / (JKS_pool / db_pool)
    cv_intercept <- qf(1 - alpha, length(group) - 1, db_pool)
    print(db_gab)
    print(db_pool)
    if (F_slope > cv_slope) {
      slope <- "Tidak Paralel"
      if (F_intercept > cv_intercept) {
        intercept <- "Tidak Berimpit"
        text_content <- paste0("H0:B1=B2=B3=...=Bk ditolak dan H0:a1=a2=a3=...=ak ditolak, Garis regresi populasinya tidak sejajar dan tetapi tidak berimpit")
      } else {
        intercept <- "Berimpit"
        text_content <- paste0("H0:B1=B2=B3=...=Bk ditolak dan H0:a1=a2=a3=...=ak tidak ditolak, Garis regresi populasinya tidak sejajar, tetapi berimpit")
      }
    } else {
      slope <- "Paralel"
      if (F_intercept > cv_intercept) {
        intercept <- "Tidak Berimpit"
        text_content <- paste0("H0:B1=B2=B3=...=Bk tidak ditolak dan H0:a1=a2=a3=...=ak ditolak, Garis regresi populasinya sejajar , tetapi tidak berimpit")
      } else {
        intercept <- "Berimpit"
        text_content <- paste0("H0:B1=B2=B3=...=Bk tidak ditolak dan H0:a1=a2=a3=...=ak tidak ditolak, Garis regresi populasinya sejajar dan berimpit")
      }
    }
    print(text_content)
    tbl_kesimpulan_2grup <- data.frame(
      Coef_a = as.character(round(unlist(a), 3)),
      Coef_B = as.character(round(unlist(B), 3)),
      SSE = as.character(round(unlist(JKS), 3)),
      stringsAsFactors = FALSE
    )
    rownames(tbl_kesimpulan_2grup) <- c(group)
    tbl_kesimpulan <- data.frame(
      F_stat_slope = c(abs(round(F_slope, 3)), ""),
      cv_slope = c(round(cv_slope, 3), ""),
      df_slope = c(paste0("(", length(group) - 1, ",", db_gab, ")"), slope),
      F_stat_intercept = c(abs(round(F_intercept, 3)), ""),
      cv_intercept = c(round(cv_intercept, 3), ""),
      df_intercept = c(paste0("(", length(group) - 1, ",", db_pool, ")"), intercept),
      stringsAsFactors = FALSE
    )
    rownames(tbl_kesimpulan) <- c("Test", "Result")

    # Create the first table
    table1 <- tbl_kesimpulan %>%
      gt() %>%
      tab_header(title = "Hypothesis Testing for Regression Lines") %>%
      cols_align(align = "right", columns = everything())

    # Create the second table
    table2 <- tbl_kesimpulan_2grup %>%
      gt() %>%
      tab_header(title = "Coefficients of Regression Lines") %>%
      cols_align(align = "right", columns = everything())
    # Combine the two tables into a single HTML document
    html_doc <- tagList(
      table1,
      table2,
      text_content
    )

    # Display the HTML document in an HTML widget
    html_print(html_doc)
  }
  p <- ggplot2::ggplot(dat, aes(x = x, y = y, color = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Comparison of Regression Lines")
  print(p)
}

slope_test(dat = dat3, alpha = .05)
