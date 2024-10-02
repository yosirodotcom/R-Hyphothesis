# Load necessary libraries
pacman::p_load(pacman, emmeans, ggplot2)

# Step 1: Set up parameters
# Simulate data
x1 <- c(0.2, 0.25, 0.25, 0.3, 0.4, 0.5, 0.5)
y1 <- c(30, 26, 40, 35, 54, 56, 65)
x2 <- c(0.2, 0.25, 0.3, 0.4, 0.4, 0.5)
y2 <- c(23, 24, 42, 49, 55, 70)

data <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  group = factor(c(rep("A", length(x1)), rep("B", length(x2))))
)

hypo_type <- function(data, test){
  model_full <- lm(y ~ x * group, data = data)

  # Step 3: View the t-value
  if(test=="slopes"){
    emm <- emmeans::emtrends(model_full, ~group, var = "x")
    conclusion1 <- "\nis inside critical region,\nreject H_0,\nthe regression lines are not paralel"
    conclusion2 <- "\nis outside critical region,\nfail to reject H_0,\nthe regression lines are paralel"
  }
  
  if(test=="intercepts"){
    emm <- emmeans::emmeans(model_full, ~group)
    conclusion1 <- "\nis inside critical region,\nreject H_0,\nthe regression lines are not coincide"
    conclusion2 <- "\nis outside critical region,\nfail to reject H_0,\nthe regression lines are coincide"
  }

  t_value <- emmeans::test(pairs(emm), null = 0)$t.ratio

  # Step 4: Calculate degrees of freedom
  df <- emmeans::test(pairs(emm), null = 0)$df

  # Calculate the critical value
  critical_value <- qt(1 - 0.05 / 2, df)

  # Step 5: Generate the t-distribution data frame
  x <- seq(-4, 4, length = 100)
  y <- dt(x, df)
  t_dist_data <- data.frame(x = x, y = y)

  # Step 6: Create the base ggplot
  p <- ggplot(t_dist_data, aes(x = x, y = y)) +
    geom_line(size = 1, color = "blue") +
    labs(title = paste("Visualization of Rejected Region (", expression(alpha), " = ", alpha, ")"),  y = "Density", x = "t-value") +
    theme_minimal() +
    geom_area(
      data = subset(t_dist_data, x >= critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    ) +
    geom_area(
      data = subset(t_dist_data, x <= -critical_value),
      aes(y = y), fill = "red", alpha = 0.5
    ) +
    geom_vline(xintercept = t_value, color = "green", linetype = "dashed", size = 1) +
    annotate("text",
      x = t_value - 0.3, y = 0.05,
      label = round(t_value, 3),
      color = "blue", vjust = -1
    ) +
    geom_curve(
      aes(
        x = t_value - 0.8, y = 0.2,
        xend = t_value - 0.1, yend = 0.07
      ),
      curvature = 0.4,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "grey"
    ) +
    annotate("text", x = critical_value, y = -0.03, label = round(critical_value, 3), color = "red", vjust = -1) +
    annotate("text", x = -critical_value, y = -0.03, label = round(-critical_value, 3), color = "red", vjust = -1)

  if (abs(t_value) > critical_value) {
    conclusion_text <- paste0("Because t_stat = ", round(t_value, 3), conclusion1)
  } else {
    conclusion_text <- paste0("Because t_stat =", round(t_value, 3), conclusion2)
  }

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
}

hypo_type(data=data, test="intercepts")
