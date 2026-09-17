# Simple RStudio Demo Script

# Create some variables
name <- "Joanna"
x <- 1:10
y <- x^2

# Display output in the Console
print(paste("Hello,", name))

# Create a data frame
demo_data <- data.frame(
  Number = x,
  Square = y
)

# View the data frame
print(demo_data)

# Open data viewer tab
View(demo_data)

# Basic calculation
sum(y)
mean(x)

# Create a plot (appears in Plots pane)
plot(
  x, y,
  type = "b",
  col = "orange",
  pch = 19,
  main = "Squares of Numbers",
  xlab = "Number",
  ylab = "Square"
)

# Show R's built-in help
?mean

# Alternative help command
help(plot)

# ==================================================
# Try typing these directly into the Console
# ==================================================

# 2 + 2
# sqrt(25)
# mean(x)
