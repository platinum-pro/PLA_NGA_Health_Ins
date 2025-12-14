library(ggplot2)
library(dplyr)

# Create data frame with P50 values
p50_data <- data.frame(
  Group = c("All", "Female", "Male", "Above Median Age", "Below Median Age",
            "High Religiosity", "Low Religiosity", 
            "Higher Income", "Lower Income", "History of Alcohol Use", 
            "No History of Alcohol Use"),
  P50 = c(6745.69, 17412.552, 3301.483, 26407.988, 3699.007,
          10599.996, 6413.621, 
          12205.409, 3472.334, 12851.241, 7123.650),
  Category = c("Overall", "Gender", "Gender", "Age", "Age",
               "Religiosity", "Religiosity",
               "Income", "Income", "Alcohol Use", "Alcohol Use")
)

# Reorder groups for better visualization (reverse order for horizontal)
p50_data$Group <- factor(p50_data$Group, 
                         levels = rev(c("All", "Female", "Male",
                                        "Above Median Age", "Below Median Age",
                                        "High Religiosity", "Low Religiosity",
                                        "Higher Income", "Lower Income",
                                        "History of Alcohol Use", "No History of Alcohol Use")))

# Create color palette
group_colors <- c("All" = "gray40",
                  "Female" = "#F8766D", "Male" = "#00BFC4",
                  "Above Median Age" = "#F8766D", "Below Median Age" = "#00BFC4",
                  "High Religiosity" = "#F8766D", "Low Religiosity" = "#00BFC4",
                  "Higher Income" = "#F8766D", "Lower Income" = "#00BFC4",
                  "History of Alcohol Use" = "#F8766D", "No History of Alcohol Use" = "#00BFC4")

# Create horizontal bar chart
p <- ggplot(p50_data, aes(x = Group, y = P50, fill = Group)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0("₦", formatC(P50, format = "f", big.mark = ",", digits = 0))),
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
# ggsave("p50_comparison_barplot.png", p, width = 10, height = 7, dpi = 300)
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
  cat(cat, ":\n", sep = "")
  cat("  ", cat_data$Group[1], ": ₦", formatC(cat_data$P50[1], format = "f", big.mark = ",", digits = 2), "\n", sep = "")
  cat("  ", cat_data$Group[2], ": ₦", formatC(cat_data$P50[2], format = "f", big.mark = ",", digits = 2), "\n", sep = "")
  diff <- cat_data$P50[1] - cat_data$P50[2]
  ratio <- cat_data$P50[1] / cat_data$P50[2]
  cat("  Difference: ₦", formatC(abs(diff), format = "f", big.mark = ",", digits = 2), 
      " (", ifelse(diff > 0, cat_data$Group[1], cat_data$Group[2]), " is ", 
      formatC(ratio, format = "f", digits = 2), "x higher)\n\n", sep = "")
}