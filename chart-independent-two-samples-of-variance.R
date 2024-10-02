# Load the ggplot2 library
library(ggplot2)

# Two Sided Hyphotesis

# Define degrees of freedom
df1 <- 9 # numerator degrees of freedom
df2 <- 14 # denominator degrees of freedom
alpha <- .1 / 2
s1 <- 0.05^2
s2 <- 0.07^2
F <- s1 / s2

# Create a data frame for the F distribution
x <- seq(0, 5, length.out = 100)
y <- df(x, df1, df2)
f_data <- data.frame(x = x, y = y)

# Define the critical values
critical_value_right <- qf(1 - alpha, df1, df2) # Right critical value (90th percentile)
critical_value_left <- qf(alpha, df1, df2) # Left critical value (10th percentile)

# Determine the conclusion text
if (F > critical_value_right | F < critical_value_left) {
  conclusion <- "Inside of the critical region,\nReject H0.\nThe variances are not equal."
} else {
  conclusion <- "Outside of the critical region,\nFail Reject H0.\nThe variances are equal."
}

# Create the F distribution plot with shaded areas
p <- ggplot(f_data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  # Shade area to the right of the right critical value
  geom_area(data = subset(f_data, x > critical_value_right), aes(y = y), fill = "red", alpha = 0.8) +
  # Shade area to the left of the left critical value
  geom_area(data = subset(f_data, x < critical_value_left), aes(y = y), fill = "red", alpha = 0.8) +
  # Add vertical line at calculated F value
  geom_vline(xintercept = round(F, 2), linetype = "dotted", color = "purple") +
  # Add curved arrow annotation
  annotate("curve",
    x = round(F, 2) + 1.9, y = 0.28,
    xend = round(F, 2), yend = 0.07,
    curvature = 0.3, arrow = arrow(length = unit(0.2, "cm")), color = "purple"
  ) +
  labs(
    title = "Visualization of Rejected Region",
    x = "F Value",
    y = "Density"
  ) +
  theme_minimal() +
  annotate("text", x = critical_value_right, y = -0.015, label = round(critical_value_right, 2), color = "black") +
  annotate("text", x = critical_value_left, y = -0.015, label = round(critical_value_left, 2), color = "black") +
  geom_label(aes(x = round(F, 2) + 2, y = 0.23, label = paste0("F = ", round(F, 3), "\n", conclusion)),
    color = "black", fill = "lightyellow", size = 4,
    label.padding = unit(0.5, "lines"), label.size = 0.5, fill = "lightblue"
  ) +
  annotate("text", x = round(F, 2) + 0.005, y = 0.05, label = round(F, 3), hjust = 0) +
  geom_rect(aes(xmin = 4, xmax = 4.2, ymin = .7, ymax = .66),
    alpha = 0.5,
    fill = "red"
  ) +
  annotate("text",
    x = 4.7,
    y = 0.68,
    label = "Critical Region",
    color = "red"
  )

print(p)
