# Load necessary libraries
library(ggplot2)



# Set up parameters
alpha <- 0.05
var_sam <- 1.855
var_pop <- 3
n <- 20
df <- n - 1
hyp_mode <- "two-sided" # options: "two-sided", "left", "right"

# Generate the t-distribution data frame
x <- seq(0, qchisq(alpha / 2, df, lower.tail = FALSE) * 1.5, length = 100)
y <- dchisq(x, df)
chi_dist_data <- data.frame(x = x, y = y)
# Create the base ggplot
p <- ggplot(chi_dist_data, aes(x = x, y = y)) +
  geom_line(size = 1, color = "blue") +
  labs(title = paste("Visualization of Rejected Region (", expression(alpha), " = ", alpha, ")"), y = "Density", x = "Chi-Square Value") +
  theme_minimal()

# Add shaded regions based on hypothesis mode
if (hyp_mode == "two-sided") {
  critical_value_right <- qchisq(alpha / 2, df, lower.tail = FALSE)

  critical_value_left <- qchisq(alpha / 2, df, lower.tail = TRUE)


  p <- p +
    geom_area(
      data = subset(chi_dist_data, x >= critical_value_right),
      aes(y = y), fill = "red", alpha = 0.5
    ) +
    geom_area(
      data = subset(chi_dist_data, x <= critical_value_left),
      aes(y = y), fill = "red", alpha = 0.5
    )
} else if (hyp_mode == "left") {
  critical_value_left <- qchisq(alpha, df, lower.tail = TRUE)

  p <- p +
    geom_area(
      data = subset(chi_dist_data, x <= critical_value_left),
      aes(y = y), fill = "red", alpha = 0.5
    )
} else if (hyp_mode == "right") {
  critical_value_right <- qchisq(alpha, df, lower.tail = FALSE)

  p <- p +
    geom_area(
      data = subset(chi_dist_data, x >= critical_value_right),
      aes(y = y), fill = "red", alpha = 0.5
    )
}

chi_value <- (df * var_sam) / (var_pop)

# Add a vertical line for the chi-squared-value
p <- p +
  geom_vline(xintercept = chi_value, color = "green", linetype = "dashed", size = 1) +
  annotate("text",
    x = chi_value - 0.3, y = 0.0005,
    label = round(chi_value, 3),
    color = "blue", vjust = -1
  )

# Step 9: Add a curved annotation from the text to the vertical line
p <- p +
  geom_curve(
    aes(
      x = chi_value + 5, y = 0.02,
      xend = chi_value + 1, yend = 0.005
    ),
    curvature = -0.4,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "grey"
  )




# Step 11: Add conclusion annotation based on hypothesis mode and t-value
if (hyp_mode == "two-sided") {
  p <- p +
    annotate("text", x = critical_value_right, y = -0.004, label = round(critical_value_right, 3), color = "red", vjust = -1) +
    annotate("text", x = critical_value_left, y = -0.004, label = round(critical_value_left, 3), color = "red", vjust = -1)

  if (chi_value > critical_value_right | chi_value < critical_value_left) {
    conclusion_text <- paste0("Because chi_square_stat = ", round(chi_value, 3), "\ninside critical region,\nreject H_0,\nthe variance sample is not\nequal to variance population")
  } else {
    conclusion_text <- paste0("Because chi_square_stat =", round(chi_value, 3), "\noutside critical region,\nfail to reject H_0,\nthe variance sample is\nequal to variance population")
  }
} else if (hyp_mode == "left") {
  p <- p +

    annotate("text", x = critical_value_left, y = -0.004, label = round(critical_value_left, 3), color = "red", vjust = -1)

  if (chi_value < critical_value_left) {
    conclusion_text <- paste0("Because chi_square_stat = ", round(chi_value, 3), "\ninside critical region,\nreject H_0,\nthe variance sample is less\nthan to variance population")
  } else {
    conclusion_text <- paste0("Because chi_square_stat = ", round(chi_value, 3), "\noutside critical region,\nfail to reject H_0,\nthe variance sample is less\nthan to variance population")
  }
} else if (hyp_mode == "right") {
  p <- p +
    annotate("text", x = critical_value_right, y = -0.004, label = round(critical_value_right, 3), color = "red", vjust = -1)

  if (chi_value > critical_value_right) {
    conclusion_text <- paste0("Because chi_square_stat = ", round(chi_value, 3), "\ninside critical region,\nreject H_0,\nthe variance sample is greater\nthan to variance population")
  } else {
    conclusion_text <- paste0("Because chi_square_stat = ", round(chi_value, 3), "\noutside critical region,\nfail to reject H_0,\nthe variance sample is not greater\nthan to variance population")
  }
}

# Step 12: Add the conclusion text as a box annotation in the top left corner
p <- p +
  geom_label(aes(x = chi_value + 5, y = 0.025, label = conclusion_text),
    color = "black", fill = "lightyellow", size = 4,
    label.padding = unit(0.5, "lines"), label.size = 0.5, fill = "lightblue"
  ) +
  geom_rect(aes(xmin = 25, xmax = 27, ymin = .05, ymax = .054),
    alpha = 0.5,
    fill = "red"
  ) +
  annotate("text",
    x = 31,
    y = .052,
    label = "Critical Region",
    color = "red"
  )

# Display the plot
print(p)
