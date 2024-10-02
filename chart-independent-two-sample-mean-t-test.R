# Load necessary libraries
library(ggplot2)

# Step 1: Set up parameters
alpha <- 0.05
sample1 <- c(6, 8, 5, 7, 4, 7, 6, 8)
sample2 <- c(5, 8, 4, 6, 3, 7, 4, 4)

# perform levene test for equality of variances
library(car)
lev <- leveneTest(sample1, sample2)
equal_var <- lev$`Pr(>F)`[1] < 0.05
print(lev)

# Step 2: Perform the two-sample t-test
t_test_result <- t.test(sample1, sample2, var.equal = !equal_var)

# Step 3: View the t-value
t_value <- t_test_result$statistic
hyp_mode <- "two-sided" # options: "two-sided", "left", "right"

# Step 4: Calculate degrees of freedom
df <- length(sample1) + length(sample2) - 2

# Step 5: Generate the t-distribution data frame
x <- seq(-4, 4, length = 100)
y <- dt(x, df)
t_dist_data <- data.frame(x = x, y = y)

# Step 6: Create the base ggplot
p <- ggplot(t_dist_data, aes(x = x, y = y)) +
  geom_line(size = 1, color = "blue") +
  labs(title = paste("Visualization of Rejected Region (", expression(alpha), " = ", alpha, ")"), subtitle = ifelse(!equal_var, "Equal variances assumed", "Equal variances not assumed"), y = "Density", x = "t-value") +
  theme_minimal()

# Step 7: Add shaded regions based on hypothesis mode
if (hyp_mode == "two-sided") {
  critical_value <- qt(1 - alpha / 2, df)

  p <- p +
    geom_area(
      data = subset(t_dist_data, x >= critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    ) +
    geom_area(
      data = subset(t_dist_data, x <= -critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    )
} else if (hyp_mode == "left") {
  critical_value <- qt(alpha, df)

  p <- p +
    geom_area(
      data = subset(t_dist_data, x <= critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    )
} else if (hyp_mode == "right") {
  critical_value <- qt(1 - alpha, df)

  p <- p +
    geom_area(
      data = subset(t_dist_data, x >= critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    )
}

# Step 8: Add a vertical line for the t-value
p <- p +
  geom_vline(xintercept = t_value, color = "green", linetype = "dashed", size = 1) +
  annotate("text",
    x = t_value - 0.3, y = 0.05,
    label = round(t_value, 3),
    color = "blue", vjust = -1
  )

# Step 9: Add a curved annotation from the text to the vertical line
p <- p +
  geom_curve(
    aes(
      x = t_value - 0.8, y = 0.2,
      xend = t_value - 0.1, yend = 0.07
    ),
    curvature = 0.4,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "grey"
  )

# Step 10: Add critical value annotation


# Step 11: Add conclusion annotation based on hypothesis mode and t-value
if (hyp_mode == "two-sided") {
  p <- p +
    annotate("text", x = critical_value, y = -0.03, label = round(critical_value, 3), color = "red", vjust = -1) +
    annotate("text", x = -critical_value, y = -0.03, label = round(-critical_value, 3), color = "red", vjust = -1)

  if (abs(t_value) > critical_value) {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), "\nis inside critical region,\nreject H_0,\nthe mean sample 1 is not\nequal to mean sample 2")
  } else {
    conclusion_text <- paste0("Because t_stat =", round(t_value, 3), "\nis outside critical region,\nfail to reject H_0,\nthe mean sample 1 is\nequal to mean sample 2")
  }
} else if (hyp_mode == "left") {
  p <- p +

    annotate("text", x = -critical_value, y = -0.03, label = round(-critical_value, 3), color = "red", vjust = -1)

  if (t_value < critical_value) {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), "\nis inside critical region,\nreject H_0,\nthe mean sample 1 is less\nthan to mean sample 2")
  } else {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), "\nis outside critical region,\nfail to reject H_0,\nthe mean sample 1 is less\nthan to mean sample 2")
  }
} else if (hyp_mode == "right") {
  p <- p +
    annotate("text", x = critical_value, y = -0.03, label = round(critical_value, 3), color = "red", vjust = -1)

  if (t_value > critical_value) {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), "\nis inside critical region,\nreject H_0,\nthe mean sample 1 is greater\nthan to mean sample 2")
  } else {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), "\nis outside critical region,\nfail to reject H_0,\nthe mean sample 1 is not greater\nthan to mean sample 2")
  }
}

# Step 12: Add the conclusion text as a box annotation in the top left corner
p <- p +
  geom_label(aes(x = t_value - 0.5, y = 0.2, label = conclusion_text),
    color = "black", fill = "lightyellow", size = 4,
    label.padding = unit(0.5, "lines"), label.size = 0.5
  ) +
  geom_rect(aes(xmin = 3, xmax = 3.2, ymin = .35, ymax = .37),
    alpha = 0.5,
    fill = "red"
  ) +
  annotate("text",
    x = 3.8,
    y = .36,
    label = "Critical Region",
    color = "red"
  )

# Display the plot
print(p)
