library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)

# Raw data
Price <- c(5, 25, 65, 125, 250, 500, 1500, 3000, 5500, 17500, 35000, 70000, 140000, 280000, 560000)
S4644 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
S5223 <- c(1, 1, 1, 1, 1, 1, 1, 1)
S0764 <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0)
S4587 <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
S2609 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

# Create separate data frames for each subject
df_4644 <- data.frame(x = Price, y = S4644, subject = "S4644")
df_5223 <- data.frame(x = Price[1:8], y = S5223[1:8], subject = "S5223")
df_0764 <- data.frame(x = Price, y = S0764, subject = "S0764")
df_4587 <- data.frame(x = Price, y = S4587, subject = "S4587")
df_2609 <- data.frame(x = Price, y = S2609, subject = "S2609")

# Combine all data frames
df_long <- rbind(df_4644, df_5223, df_0764, df_4587, df_2609)

# Create mapping for participant names
participant_names <- c("S4644" = "Participant 1",
                       "S5223" = "Participant 2",
                       "S0764" = "Participant 3",
                       "S4587" = "Participant 4",
                       "S2609" = "Participant 5")

# Define colors for each participant
participant_colors <- c("S4644" = "#E41A1C",  # red
                        "S5223" = "#377EB8",  # blue
                        "S0764" = "#4DAF4A",  # green
                        "S4587" = "#984EA3",  # purple
                        "S2609" = "#FF7F00")  # orange

# Plot function with line and color
plot_with_line <- function(subject_data, subject_code) {
  subject_name <- participant_names[subject_code]
  subject_color <- participant_colors[subject_code]
  
  ggplot(subject_data, aes(x = x, y = y)) +
    geom_point(size = 2, color = subject_color) +
    geom_line(color = subject_color) +
    scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000),
                  labels = function(x) paste0(scales::comma(x))) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = subject_name,
         x = "Price (₦)",
         y = "Willingness to purchase\nhealth insurance") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5)) +
    annotation_logticks(sides = "b")
}

# Create plots in the correct order
plot_order <- c("S4644", "S5223", "S0764", "S4587", "S2609")
plots <- lapply(plot_order, function(subject_code) {
  data <- df_long[df_long$subject == subject_code, ]
  plot_with_line(data, subject_code)
})

# Display plots
gridExtra::grid.arrange(grobs = plots, ncol = 3)