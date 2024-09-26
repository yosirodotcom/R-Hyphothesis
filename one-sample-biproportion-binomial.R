# Load necessary libraries
library(ggplot2)
library(grid)

# Define parameters for the binomial distribution
n <- 18  # number of trials
p <- 0.2 # probability of success
right_tail <- FALSE
cv <- 1 # critical value
x_stat <- 0 # value for vertical line

# Create a data frame for the binomial distribution
x <- 0:n  # possible number of successes
probabilities <- dbinom(x, size = n, prob = p)  # calculate probabilities
cumulative_probabilities <- pbinom(x, size = n, prob = p)  # calculate cumulative probabilities
binom_data <- data.frame(successes = x, probabilities = probabilities, cumulative_probabilities = cumulative_probabilities)

# Find the maximum probability for positioning the label
max_prob <- max(probabilities)

# Determine the text to display based on the conditions
if (right_tail && x_stat > cv) {
  text_content <- "Because x_stat is in the rejected region, we must reject H_0. The proportion of the sample is bigger than the population."
} else if (right_tail && x_stat <= cv) {
  text_content <- "Because x_stat is not in the rejected region, we fail to reject H_0. The proportion of the sample is not bigger than the population."
} else if (!right_tail && x_stat > cv) {
  text_content <- "Because x_stat is not in the rejected region, we fail to reject H_0. The proportion of the sample is not smaller than the population."
} else {
  text_content <- "Because x_stat is in the rejected region, we must reject H_0. The proportion of the sample is smaller than the population."
}

# Create the binomial distribution plot
p <- ggplot(binom_data, aes(x = successes, y = probabilities)) +
  geom_bar(stat = "identity", 
           aes(fill = ifelse((right_tail & successes >= cv) | 
                               (!right_tail & successes <= cv), 
                             "red", "skyblue")), 
           color = NA) +  # Set color to NA to remove bar outlines
  labs(title = "Visualization of Rejected Region",
       x = "Number of Successes",
       y = "Probability") +
  scale_x_continuous(breaks = 0:n) +  # Set x-axis breaks to include all successes
  scale_fill_identity() +  # Use the fill colors defined in aes
  theme_minimal() +
  geom_vline(xintercept = x_stat, linetype = "dashed", color = "blue") +  # Add vertical dashed line
  annotate("text", x = x_stat + 2, y = max_prob + 0.02,  # Position label above the highest bar
           label = expression(x[stat] == 13), color = "blue") +  # Use expression for x_stat
  annotate("curve", x = x_stat + 2, y = max_prob + 0.015, 
           xend = x_stat, yend = max_prob, 
           curvature = -0.4, arrow = arrow(length = unit(0.2, "cm")), 
           color = "blue", linetype = 1) 

# Add cumulative probabilities to the plot (replacing probability labels)
if (right_tail) {
  p <- p + geom_text(aes(label = round(1 - cumulative_probabilities, 3)), 
                     vjust = -0.5, color = "grey", size = 3.5) 
} else {
  p <- p + geom_text(aes(label = round(cumulative_probabilities, 3)), 
                     vjust = -0.5, color = "grey", size = 3.5)
}

# Print the plot
print(p)

# Function to split text into multiple lines
split_text <- function(text, max_words_per_line) {
  words <- unlist(strsplit(text, " "))
  
  # Create lines by grouping words
  lines <- sapply(seq(1, length(words), by = max_words_per_line), 
                  function(i) paste(words[i:min(i + max_words_per_line - 1, length(words))], collapse = " "))
  
  return(lines)
}

# Prepare the text for display
formatted_text <- split_text(text_content, 5)

# Use grid.text to add the text to the plot area
grid.text(label = paste(formatted_text, collapse = "\n"), 
          x = unit(0.95, "npc"),  # Position at 95% of the plot width
          y = unit(0.5, "npc"),   # Position at 50% of the plot height
          just = "right",         # Right align the text
          gp = gpar(col = "black", fontsize = 10, fill = "lightblue"))
