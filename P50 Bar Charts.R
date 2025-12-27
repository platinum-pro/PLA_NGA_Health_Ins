library(ggplot2)
library(dplyr)

# Create data frame with P50 values from the new analysis
p50_data <- data.frame(
  Group = c("All",
            "Access to multiple sellers", "Access to single seller",
            "Female", "Male", 
            "Above median age", "Below median age",
            "High religiosity", "Low religiosity", 
            "Higher income", "Lower income", 
            "History of drinking alcohol", "No history of drinking alcohol",
            "History of gambling", "No history of gambling"),
  P50 = c(3299.75,
          3694.87, 3591.48,
          7750.12, 3021.95,
          6279.21, 2571.22,
          8829.97, 905.75,
          6201.31, 2062.11,
          2309.02, 4686.89,
          10524.20, 3884.17),
  Category = c("Overall",
               "Economy", "Economy",
               "Gender", "Gender", 
               "Age", "Age",
               "Religiosity", "Religiosity",
               "Income", "Income", 
               "Alcohol Use", "Alcohol Use",
               "Gambling", "Gambling")
)

# Reorder groups for better visualization (reverse order for horizontal)
p50_data$Group <- factor(p50_data$Group, 
                         levels = rev(c("All",
                                        "Access to multiple sellers", "Access to single seller",
                                        "Female", "Male",
                                        "Above median age", "Below median age",
                                        "High religiosity", "Low religiosity",
                                        "Higher income", "Lower income",
                                        "History of drinking alcohol", "No history of drinking alcohol",
                                        "History of gambling", "No history of gambling")))

# Create color palette
group_colors <- c("All" = "gray40",
                  "Access to multiple sellers" = "#F8766D", "Access to single seller" = "#00BFC4",
                  "Female" = "#F8766D", "Male" = "#00BFC4",
                  "Above median age" = "#F8766D", "Below median age" = "#00BFC4",
                  "High religiosity" = "#F8766D", "Low religiosity" = "#00BFC4",
                  "Higher income" = "#F8766D", "Lower income" = "#00BFC4",
                  "History of drinking alcohol" = "#F8766D", "No history of drinking alcohol" = "#00BFC4",
                  "History of gambling" = "#F8766D", "No history of gambling" = "#00BFC4")

# Create horizontal bar chart
p <- ggplot(p50_data, aes(x = Group, y = P50, fill = Group)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0("₦", formatC(P50, format = "f", big.mark = ",", digits = 2))),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0, max(p50_data$P50) * 1.2),
                     expand = c(0, 0)) +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "P50 (₦)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 10, r = 40, b = 10, l = 10, unit = "pt")
  )

# Display the plot
print(p)

# Save the plot
# ggsave("p50_comparison_barplot.png", p, width = 10, height = 8, dpi = 300)
# cat("\nBar chart saved as 'p50_comparison_barplot.png'\n")

# Print summary statistics
cat("\n######################################\n")
cat("P50 SUMMARY STATISTICS\n")
cat("######################################\n\n")

# Overall comparison
cat("OVERALL: ₦", formatC(p50_data$P50[p50_data$Group == "All"], 
                          format = "f", big.mark = ",", digits = 2), "\n\n", sep = "")

# Calculate differences within categories
categories <- unique(p50_data$Category[p50_data$Category != "Overall"])
for(cat in categories) {
  cat_data <- p50_data[p50_data$Category == cat, ]
  cat_data <- cat_data[order(cat_data$P50, decreasing = TRUE), ]
  
  cat(cat, ":\n", sep = "")
  cat("  ", cat_data$Group[1], ": ₦", formatC(cat_data$P50[1], format = "f", big.mark = ",", digits = 2), "\n", sep = "")
  cat("  ", cat_data$Group[2], ": ₦", formatC(cat_data$P50[2], format = "f", big.mark = ",", digits = 2), "\n", sep = "")
  diff <- cat_data$P50[1] - cat_data$P50[2]
  ratio <- cat_data$P50[1] / cat_data$P50[2]
  cat("  Difference: ₦", formatC(abs(diff), format = "f", big.mark = ",", digits = 2), 
      " (", cat_data$Group[1], " is ", 
      formatC(ratio, format = "f", digits = 2), "x higher)\n\n", sep = "")
}