library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Read the data
data <- read.csv("~/Downloads/Agg_percentage_analysis.csv")

# Convert All column to numeric if needed
data$All <- as.numeric(data$All)

# Define the Koff function
Koff <- function(x, alpha, Qo, data) {
  # Calculate k from the range of observed Q values (dependent variable)
  valid_q <- data$All[!is.na(data$All) & data$All > 0]
  k <- log10(max(valid_q)) - log10(min(valid_q))
  Qo * 10^(k * (exp(-alpha * Qo * x) - 1))
}

# Function to calculate P50 using numerical root-finding
calculate_p50 <- function(alpha, Qo, data, target_q = 50) {
  # Calculate k from the range of observed Q values (dependent variable)
  valid_q <- data$All[!is.na(data$All) & data$All > 0]
  k <- log10(max(valid_q)) - log10(min(valid_q))
  
  # Add check for valid inputs
  if(alpha <= 0 || Qo <= 0 || k <= 0) {
    return(NA)
  }
  
  # Define the function we want to find the root of: Q(P) - target_q = 0
  objective_function <- function(price) {
    q_pred <- Qo * 10^(k * (exp(-alpha * Qo * price) - 1))
    return(q_pred - target_q)
  }
  
  # Check if Q0 is above or below the target
  q_at_zero <- Qo * 10^(k * (exp(0) - 1))  # Q at price = 0
  
  if(q_at_zero < target_q) {
    warning("Q0 is below target Q - P50 cannot be calculated")
    return(NA)
  }
  
  # Define search interval (from minimum to maximum observed prices)
  lower_bound <- min(data$x[data$x > 0], na.rm = TRUE)
  upper_bound <- max(data$x, na.rm = TRUE)
  
  # Check if the function crosses the target within our range
  f_lower <- objective_function(lower_bound)
  f_upper <- objective_function(upper_bound)
  
  if(f_lower * f_upper > 0) {
    warning("Target Q is not crossed within the observed price range")
    return(NA)
  }
  
  # Use uniroot to find P50
  result <- tryCatch({
    uniroot(objective_function, 
            interval = c(lower_bound, upper_bound),
            tol = 0.01)  # Tolerance for convergence
  }, error = function(e) {
    warning("Error in root-finding: ", e$message)
    return(NULL)
  })
  
  if(!is.null(result)) {
    return(result$root)
  } else {
    return(NA)
  }
}

# Function to fit model and calculate R-squared
fit_and_calculate_rsq <- function(data) {
  # Remove NA values
  valid_data <- data[!is.na(data$All) & data$x > 0, ]
  
  fit <- tryCatch({
    nls(formula = All ~ Koff(x, alpha, Qo, data),
        data = valid_data,
        start = list(alpha = 0.0000001, Qo = 100),
        algorithm = "port",
        lower = c(alpha = 0, Qo = 0),
        upper = c(alpha = 0.1, Qo = 100),
        control = nls.control(maxiter = 50000))
  }, error = function(e) {
    message("Error fitting model: ", e$message)
    return(NULL)
  })
  
  if (!is.null(fit)) {
    residuals <- residuals(fit)
    tss <- sum((valid_data$All - mean(valid_data$All, na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residuals^2)
    r_squared <- 1 - (rss / tss)
    
    # Get parameters
    params <- coef(fit)
    
    # Calculate P50
    p50 <- calculate_p50(params["alpha"], params["Qo"], valid_data, target_q = 50)
    
    # Calculate k for display
    valid_q <- valid_data$All[!is.na(valid_data$All) & valid_data$All > 0]
    k <- log10(max(valid_q)) - log10(min(valid_q))
    
    cat("R-squared =", round(r_squared, 4), "\n")
    cat("k =", round(k, 4), "\n")
    cat("alpha =", format(params["alpha"], scientific = TRUE), "\n")
    cat("Q0 =", round(params["Qo"], 2), "\n")
    
    if(!is.na(p50)) {
      cat("P50 =", round(p50, 2), "\n")
    } else {
      cat("P50 could not be calculated\n")
    }
    
    return(list(fit = fit, r_squared = r_squared, p50 = p50))
  }
  
  return(NULL)
}

# Create plot function
create_plot <- function(data, fit_results) {
  # Remove zero or negative x values
  valid_data <- data[data$x > 0, ]
  x_range <- 10^seq(log10(min(valid_data$x)), log10(max(valid_data$x)), length.out = 100)
  plot_data <- data.frame(x = x_range)
  
  if (!is.null(fit_results$fit)) {
    plot_data$All_pred <- predict(fit_results$fit, newdata = data.frame(x = x_range))
  }
  
  p <- ggplot() +
    geom_point(data = valid_data, aes(x = x, y = All), shape = 16) +
    geom_line(data = plot_data, aes(x = x, y = All_pred))
  
  # Add horizontal line at Q = 50%
  p <- p + geom_hline(yintercept = 50, linetype = "dotted", 
                      color = "grey50", alpha = 0.7)
  
  # Add vertical line for P50 and horizontal/vertical intersection if P50 exists
  if (!is.null(fit_results$p50) && !is.na(fit_results$p50)) {
    if(fit_results$p50 >= min(valid_data$x) && fit_results$p50 <= max(valid_data$x)) {
      p <- p + 
        geom_vline(xintercept = fit_results$p50, linetype = "dashed", 
                   color = "blue", alpha = 0.7) +
        # Add a point at the intersection
        geom_point(aes(x = fit_results$p50, y = 50), 
                   color = "blue", size = 3, shape = 21, fill = "white", stroke = 1.5) +
        # Add annotation for P50
        annotate("text", x = fit_results$p50, y = max(valid_data$All, na.rm = TRUE) * 0.95,
                 label = paste0("P* = ₦", formatC(fit_results$p50, format = "f", big.mark = ",", digits = 2)),
                 hjust = -0.1, vjust = 1)
    }
  }
  
  p <- p +
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                  labels = scales::comma,
                  limits = range(valid_data$x)) +
    labs(title = "",
         x = "Price (₦)",
         y = "Proportion of respondents purchasing \nat each price (%)") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.margin = margin(t = 10, r = 30, b = 10, l = 10, unit = "pt")) +
    annotation_logticks(sides = "b")
  
  return(p)
}

# Fit model and calculate R-squared
fit_results <- fit_and_calculate_rsq(data)

# Print parameter values
if (!is.null(fit_results$fit)) {
  cat("\nParameters:\n")
  print(summary(fit_results$fit)$parameters)
}

# Create and display plot
p <- create_plot(data, fit_results)
print(p)