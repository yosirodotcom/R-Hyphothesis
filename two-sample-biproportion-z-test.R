# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set parameters
alpha <- 0.05
mode_hyp <- "right"  # Change to "right", "left", or "two-sided" as needed
mean <- 0
sd <- 1

# Given values
x1 <- 120 # Number of successes of group 1
n1 <- 200 # Sample size of group 1
x2 <- 240 # Number of successes of group 2
n2 <- 500 # Sample size of group 2

# Calculate the pooled proportion
p_pool <- (x1 + x2) / (n1 + n2)

# Calculate the standard error
SE <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))

# Step 3: Calculate the z-value
z <- (p_hat1 - p_hat2) / SE
print(z)

# Calculate critical values based on alpha
if (mode_hyp == "right") {
  critical_value <- qnorm(1 - alpha, mean, sd)
} else if (mode_hyp == "left") {
  critical_value <- qnorm(alpha, mean, sd)
} else if (mode_hyp == "two-sided") {
  critical_value_left <- qnorm(alpha / 2, mean, sd)
  critical_value_right <- qnorm(1 - alpha / 2, mean, sd)
}

# Create the normal distribution plot
p <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = mean, sd = sd), color = "blue") +
  labs(
    title = paste("Visualization of Rejected Region (", expression(alpha), " = ", alpha, ")"),
    x = "Z Value",
    y = "Density"
  ) +
  theme_minimal()

# Add shading based on hypothesis mode
if (mode_hyp == "right") {
  p <- p + geom_area(
    stat = "function", fun = dnorm,
    args = list(mean = mean, sd = sd),
    xlim = c(critical_value, 4),
    fill = "red", alpha = 0.5
  ) +
    geom_vline(xintercept = z, color = "green", linetype = "dashed", size = 1) +
    geom_text(aes(x = critical_value, y = -0.02, label = round(critical_value, 3)),
      color = "red", vjust = -1
    ) +
    geom_text(aes(x = z - 0.1, y = 0.01, label = round(z, 3)),
      color = "blue", vjust = -1
    ) +
    annotate("curve",
      x = z - 1, y = 0.1,
      xend = z - 0.2, yend = 0.03,
      curvature = 0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z > critical_value) {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is rejected,\nthe proportion sample 1\nis greater than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is failed to rejected,\nthe proportion sample 1\nis not greater than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  }
} else if (mode_hyp == "left") {
  p <- p + geom_area(
    stat = "function", fun = dnorm,
    args = list(mean = mean, sd = sd),
    xlim = c(-4, critical_value),
    fill = "red", alpha = 0.5
  ) +
    geom_vline(xintercept = z, color = "green", linetype = "dashed", size = 1) +
    geom_text(aes(x = critical_value, y = -0.02, label = round(critical_value, 3)),
      color = "red", vjust = -1
    ) +
    geom_text(aes(x = z - 0.1, y = 0.01, label = round(z, 3)),
      color = "blue", vjust = -1
    ) +
    annotate("curve",
      x = z - 1, y = 0.1,
      xend = z - 0.2, yend = 0.03,
      curvature = 0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z < critical_value) {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is rejected,\nthe proportion sample 1\nis less than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is failed to rejected,\nthe proportion sample 1\nis not less than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  }
} else if (mode_hyp == "two-sided") {
  p <- p + geom_area(
    stat = "function", fun = dnorm,
    args = list(mean = mean, sd = sd),
    xlim = c(-4, critical_value_left),
    fill = "red", alpha = 0.5
  ) +
    geom_area(
      stat = "function", fun = dnorm,
      args = list(mean = mean, sd = sd),
      xlim = c(critical_value_right, 4),
      fill = "red", alpha = 0.5
    ) +
    geom_vline(xintercept = z, color = "green", linetype = "dashed", size = 1) +
    geom_text(aes(x = critical_value_left, y = -0.02, label = round(critical_value_left, 2)),
      color = "red", vjust = -1
    ) +
    geom_text(aes(x = critical_value_right, y = -0.02, label = round(critical_value_right, 2)),
      color = "red", vjust = -1
    ) +
    geom_text(aes(x = z - 0.1, y = 0.01, label = round(z, 3)),
      color = "blue", vjust = -1
    ) +
    annotate("curve",
      x = z - 1, y = 0.1,
      xend = z - 0.2, yend = 0.03,
      curvature = 0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z < critical_value_left | z > critical_value_right) {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is rejected,\nthe proportion sample 1\nis not equal than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z-.2, y = 0.13, label = paste0("Z-stat = ",round(z, 3),"\nH_0 is failed to rejected,\nthe proportion sample 1\nis equal than\nproportion sample 2"), hjust = 1, fill = "lightblue")
  }
}

p <- p +
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

# Print the plot
print(p)
