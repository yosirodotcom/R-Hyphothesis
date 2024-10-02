# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set parameters
alpha <- 0.05
mode_hyp <- "two-sided" # Change to "right", "left", or "two-sided" as needed
mean <- 0
sd <- 1

# Define the variables
sample_mean <- 83 # Sample mean (X̄)
population_mean <- 80 # Population mean (μ)
population_sd <- 7 # Population standard deviation (σ)
sample_size <- 100 # Sample size (n)

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the z-score
z <- (sample_mean - population_mean) / standard_error

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
    geom_text(aes(x = z - 0.05, y = 0.02, label = round(z, 3)),
      color = "blue", vjust = -1, hjust = 1
    ) +
    annotate("curve",
      x = z - 0.9, y = 0.15,
      xend = z - .2, yend = 0.04,
      curvature = -0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z > critical_value) {
    p <- p + annotate("label", x = z - .8, y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\ninside critical region,\nrejected H_0,\nthe mean sample is\ngreater than mean population"), hjust = 1, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z - .8, y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\noutside critical region,\nfail to reject H_0,\nthe mean sample is\ngreater than mean population"), hjust = 1, fill = "lightblue")
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
    geom_text(aes(x = z + 0.05, y = 0.02, label = round(z, 3)),
      color = "blue", vjust = -1, hjust = 0
    ) +
    annotate("curve",
      x = z + 0.9, y = 0.15,
      xend = z, yend = 0.04,
      curvature = 0.3,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z < critical_value) {
    p <- p + annotate("label", x = z + .8, y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\ninside critical region,\nrejected H_0,\nthe mean sample is\nless than mean population"), hjust = 0, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z + .8, , y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\noutside critical region,\nfail to reject H_0,\nthe mean sample is\nless than mean population"), hjust = 0, fill = "lightblue")
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
    geom_text(aes(x = z - 0.05, y = 0.02, label = round(z, 3)),
      color = "blue", vjust = -1, hjust = 1
    ) +
    annotate("curve",
      x = z - 0.9, y = 0.15,
      xend = z - .2, yend = 0.04,
      curvature = -0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "blue"
    )

  # Add text annotation based on z-value and mode_hyp
  if (z < critical_value_left | z > critical_value_right) {
    p <- p + annotate("label", x = z - .8, y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\ninside critical region,\nrejected H_0,\nthe mean sample is\nnot equal to mean population"), hjust = 1, fill = "lightblue")
  } else {
    p <- p + annotate("label", x = z - .8, y = 0.15, label = paste0("Because Z_stat = ", round(z, 3), "\noutside critical region,\nfail to reject H_0,\nthe mean sample is\nequal to mean population"), hjust = 1, fill = "lightblue")
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
