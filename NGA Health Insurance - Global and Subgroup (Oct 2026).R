# =============================================================================
# HIPT DEMAND CURVES, BUILT FROM THE CLEANED PARTICIPANT-LEVEL DATASET
#
# Replaces the pre-aggregated "Agg_Percentage_Analysis" file. The percentage of
# "Yes" at each price is computed here from the same cleaned CSV and the same
# exclusions as the final-run script, so the figures and the models share a sample.
#
# Outputs (in hipt_outputs/):
#   demand_proportions_by_price.csv   % Yes, n, and Yes count for every series and price
#   demand_curves_grid.png            8-panel group comparison figure
#   demand_curve_global.png           standalone global figure
#   demand_fit_summary.csv            Q0, alpha, P50, R-squared for every curve
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(grid)
})

# -----------------------------------------------------------------------------
# 0. SETTINGS
# -----------------------------------------------------------------------------
DATA_PATH <- Sys.getenv("HIPT_DATA", "~/Downloads/Health_Insurance_recoded - 28_Sep_2026.csv")
OUT_DIR   <- "hipt_outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

EXCLUDE_UNDER18  <- TRUE     # same as the final-run script
K_GLOBAL         <- 2        # k fixed at 2 for every curve, as in the original code
MIN_N_PER_POINT  <- 1        # a price point is dropped if fewer participants answered it
GROUP_COLS       <- c("#2a6ebb", "#d95f02")   # group coded 1, group coded 0

price_cols   <- c("P5", "P25", "P65", "P125", "P250", "P500", "P1500",
                  "P3000", "P5500", "P17500", "P35000", "P70000",
                  "P140000", "P280000", "P560000")
price_values <- c(5, 25, 65, 125, 250, 500, 1500, 3000, 5500,
                  17500, 35000, 70000, 140000, 280000, 560000)

# Code 1 / code 0 labels for each comparison.
# GENDER: the final-run script codes 0 = Female and 1 = Male, which matches the
# manuscript's 29 men and 22 women. The earlier plotting code had these reversed.
# VERIFY against the Qualtrics export before using the gender panel.
demographic_pairs <- list(
  list(var = "Economy",         label1 = "Open Economy", label0 = "Closed Economy"),
  list(var = "GENDER",          label1 = "Male",                       label0 = "Female"),
  list(var = "AGE_binary",      label1 = "Above median age",           label0 = "Below median age"),
  list(var = "RELIGION_binary", label1 = "High religiosity",           label0 = "Low religiosity"),
  list(var = "INC",             label1 = "Higher income",              label0 = "Lower income"),
  list(var = "HINS",            label1 = "Insured",                    label0 = "Uninsured"),
  list(var = "ALC",             label1 = "History of drinking alcohol", label0 = "No history of drinking alcohol"),
  list(var = "GB",              label1 = "History of gambling",        label0 = "No history of gambling")
)
panel_titles <- c(Economy = "Economy Type", GENDER = "Gender", AGE_binary = "Age",
                  RELIGION_binary = "Religiosity", INC = "Income",
                  HINS = "Insurance Status", ALC = "Alcohol Use", GB = "Gambling")

# -----------------------------------------------------------------------------
# 1. LOAD, EXCLUDE, DEFINE THE SAMPLE
# -----------------------------------------------------------------------------
df <- read_csv(DATA_PATH, show_col_types = FALSE)
stopifnot(!anyDuplicated(df$ResponseId))
need <- c("Economy", "GENDER", "AGE", "RELIGION", "INC", "HINS", "ALC", "GB", price_cols)
stopifnot(all(need %in% names(df)))

if (EXCLUDE_UNDER18) df <- df %>% filter(is.na(AGE) | AGE >= 18)

resp_all <- as.matrix(df[, price_cols])
stopifnot(all(resp_all %in% c(0, 1, NA)))        # 1 = Yes, 0 = No

df <- df %>% mutate(n_resp = rowSums(!is.na(resp_all)),
                    has_econ = Economy %in% c(0, 1))

# Primary sample: at least one price response and a condition assigned (matches the models)
base <- df %>% filter(n_resp >= 1, has_econ)
cat("Primary sample: N =", nrow(base), "\n\n")

# Median splits are computed in the primary sample. Ties at the median fall in the
# lower group. CONFIRM this matches how the original binary variables were built.
base <- base %>% mutate(
  AGE_binary      = ifelse(is.na(AGE),      NA, as.integer(AGE      > median(AGE,      na.rm = TRUE))),
  RELIGION_binary = ifelse(is.na(RELIGION), NA, as.integer(RELIGION > median(RELIGION, na.rm = TRUE)))
)

cat("Group sizes (participants with a value on the splitting variable):\n")
for (p in demographic_pairs) {
  cat(sprintf("  %-32s n = %3d | %-32s n = %3d\n",
              p$label1, sum(base[[p$var]] %in% 1), p$label0, sum(base[[p$var]] %in% 0)))
}

# -----------------------------------------------------------------------------
# 2. PROPORTION OF "YES" AT EACH PRICE
#    Denominator at each price = participants in the group who answered that price.
# -----------------------------------------------------------------------------
yes_by_price <- function(d) {
  m   <- as.matrix(d[, price_cols])
  n   <- colSums(!is.na(m))
  yes <- colSums(m == 1, na.rm = TRUE)
  val <- ifelse(n >= MIN_N_PER_POINT & n > 0, 100 * yes / n, NA_real_)
  tibble(x = price_values, n = as.integer(n), yes = as.integer(yes), value = val)
}

agg_long <- bind_rows(
  yes_by_price(base) %>% mutate(series = "All", group = "All participants"),
  bind_rows(lapply(demographic_pairs, function(p) {
    bind_rows(
      yes_by_price(base[base[[p$var]] %in% 1, ]) %>%
        mutate(series = paste0(p$var, "_1"), group = p$label1),
      yes_by_price(base[base[[p$var]] %in% 0, ]) %>%
        mutate(series = paste0(p$var, "_0"), group = p$label0))
  }))
)
write_csv(agg_long, file.path(OUT_DIR, "demand_proportions_by_price.csv"))

# Wide format with columns x, All, Economy_1, Economy_0, ... (as the original code expects)
data <- agg_long %>% select(series, x, value) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  as.data.frame()

cat("\nOverall % Yes by price:\n")
print(agg_long %>% filter(series == "All") %>% select(x, n, yes, value), n = Inf)

# -----------------------------------------------------------------------------
# 3. MODEL AND HELPERS (unchanged from the original code)
# -----------------------------------------------------------------------------
Koff <- function(x, alpha, Qo, k_fixed) {
  Qo * 10^(k_fixed * (exp(-alpha * Qo * x) - 1))
}

calculate_p50 <- function(alpha, Qo, k_fixed, data_subset, target_q = 50) {
  if (is.na(k_fixed) || alpha <= 0 || Qo <= 0 || k_fixed <= 0) return(NA)
  objective_function <- function(price) Qo * 10^(k_fixed * (exp(-alpha * Qo * price) - 1)) - target_q
  q_at_zero <- Qo * 10^(k_fixed * (exp(0) - 1))
  if (q_at_zero < target_q) {
    warning("Q0 is below target Q - P50 cannot be calculated for this group")
    return(NA)
  }
  lower_bound <- min(data_subset$x[data_subset$x > 0], na.rm = TRUE)
  upper_bound <- max(data_subset$x, na.rm = TRUE)
  if (objective_function(lower_bound) * objective_function(upper_bound) > 0) {
    warning("Target Q is not crossed within the observed price range for this group")
    return(NA)
  }
  result <- tryCatch(uniroot(objective_function, interval = c(lower_bound, upper_bound), tol = 0.01),
                     error = function(e) NULL)
  if (!is.null(result)) result$root else NA
}

fit_curve <- function(d, label, k_fixed) {
  valid <- d[!is.na(d$value) & d$x > 0, ]
  if (nrow(valid) < 3) { message("Not enough data for ", label); return(NULL) }
  fit <- tryCatch(
    nls(value ~ Koff(x, alpha, Qo, k_fixed), data = valid,
        start = list(alpha = 0.0000001, Qo = 100), algorithm = "port",
        lower = c(alpha = 0, Qo = 0), upper = c(alpha = 0.1, Qo = 100),
        control = nls.control(maxiter = 50000)),
    error = function(e) { message("Error fitting ", label, ": ", e$message); NULL })
  if (is.null(fit)) return(NULL)
  prm <- coef(fit)
  r2  <- 1 - sum(residuals(fit)^2) / sum((valid$value - mean(valid$value))^2)
  p50 <- calculate_p50(prm["alpha"], prm["Qo"], k_fixed, valid)
  cat(sprintf("%s: R2 = %.3f | alpha = %s | Q0 = %.1f | P50 = %s\n", label, r2,
              format(prm["alpha"], scientific = TRUE, digits = 3), prm["Qo"],
              ifelse(is.na(p50), "NA", paste0("₦", formatC(p50, format = "f", big.mark = ",", digits = 0)))))
  list(fit = fit, r_squared = r2, p50 = p50, label = label,
       alpha = unname(prm["alpha"]), Qo = unname(prm["Qo"]), k = k_fixed)
}

axis_theme <- theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "bottom", legend.text = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))

x_scale <- scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = scales::comma)
y_scale <- scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))

# -----------------------------------------------------------------------------
# 4. GLOBAL CURVE (fit first, because it is overlaid on every panel)
# -----------------------------------------------------------------------------
cat("\n===== GLOBAL CURVE =====\n")
all_d   <- data.frame(x = data$x, value = data$All)
fit_all <- fit_curve(all_d, "All participants", K_GLOBAL)
if (is.null(fit_all)) stop("The global curve did not fit. Check the starting values.")

# -----------------------------------------------------------------------------
# 5. GROUP COMPARISON PANELS
# -----------------------------------------------------------------------------
x_range <- 10^seq(log10(min(price_values)), log10(max(price_values)), length.out = 200)
overall_pred <- Koff(x_range, fit_all$alpha, fit_all$Qo, K_GLOBAL)

make_panel <- function(pair, fit1, fit0) {
  lab1 <- pair$label1; lab0 <- pair$label0
  cols <- setNames(GROUP_COLS, c(lab1, lab0))
  d1 <- data.frame(x = data$x, value = data[[paste0(pair$var, "_1")]]) %>% filter(!is.na(value))
  d0 <- data.frame(x = data$x, value = data[[paste0(pair$var, "_0")]]) %>% filter(!is.na(value))
  
  p <- ggplot() +
    geom_line(data = data.frame(x = x_range, y = overall_pred), aes(x, y),
              color = "black", linewidth = 0.6, linetype = "dashed")
  # 50% reference line (commented out; restore by adding "+" after the line above)
  # geom_hline(yintercept = 50, linetype = "dotted", color = "grey50", alpha = 0.7)
  
  if (nrow(d1) > 0) p <- p + geom_point(data = d1, aes(x, value, color = lab1), shape = 16, alpha = 0.7, size = 1.8)
  if (nrow(d0) > 0) p <- p + geom_point(data = d0, aes(x, value, color = lab0), shape = 17, alpha = 0.7, size = 1.8)
  if (!is.null(fit1)) {
    p <- p + geom_line(data = data.frame(x = x_range, y = Koff(x_range, fit1$alpha, fit1$Qo, K_GLOBAL)),
                       aes(x, y, color = lab1), linewidth = 0.8)
    # P50 line (commented out)
    # if (!is.na(fit1$p50)) p <- p + geom_vline(xintercept = fit1$p50, linetype = "dashed", color = GROUP_COLS[1], alpha = 0.4)
  }
  if (!is.null(fit0)) {
    p <- p + geom_line(data = data.frame(x = x_range, y = Koff(x_range, fit0$alpha, fit0$Qo, K_GLOBAL)),
                       aes(x, y, color = lab0), linewidth = 0.8)
    # P50 line (commented out)
    # if (!is.na(fit0$p50)) p <- p + geom_vline(xintercept = fit0$p50, linetype = "dashed", color = GROUP_COLS[2], alpha = 0.4)
  }
  p + x_scale + y_scale +
    scale_color_manual(values = cols, name = "", breaks = c(lab1, lab0)) +
    labs(title = panel_titles[[pair$var]], x = "Price (₦)", y = "% Purchasing") +
    annotation_logticks(sides = "b", size = 0.3) +
    axis_theme
}

all_results <- list()
for (pair in demographic_pairs) {
  cat("\n=====", pair$var, "=====\n")
  f1 <- fit_curve(data.frame(x = data$x, value = data[[paste0(pair$var, "_1")]]), pair$label1, K_GLOBAL)
  f0 <- fit_curve(data.frame(x = data$x, value = data[[paste0(pair$var, "_0")]]), pair$label0, K_GLOBAL)
  all_results[[pair$var]] <- list(g1 = f1, g0 = f0, plot = make_panel(pair, f1, f0))
}

grid_plots <- lapply(names(panel_titles), function(v) all_results[[v]]$plot)
grid_combined <- arrangeGrob(grobs = grid_plots, ncol = 2, nrow = 4)
grid.newpage(); grid.draw(grid_combined)
ggsave(file.path(OUT_DIR, "demand_curves_grid.png"), grid_combined, width = 10, height = 16, dpi = 300)

# -----------------------------------------------------------------------------
# 6. STANDALONE GLOBAL FIGURE
# -----------------------------------------------------------------------------
obs_all <- all_d %>% filter(!is.na(value))
stats_txt <- sprintf("N = %d | Q0 = %.1f%% | alpha = %s | P50 = %s | R² = %.2f",
                     nrow(base), fit_all$Qo, format(fit_all$alpha, scientific = TRUE, digits = 3),
                     ifelse(is.na(fit_all$p50), "NA",
                            paste0("₦", formatC(fit_all$p50, format = "f", big.mark = ",", digits = 0))),
                     fit_all$r_squared)

p_global <- ggplot() +
  # 50% line and P50 line (commented out)
  # geom_hline(yintercept = 50, linetype = "dotted", color = "grey50") +
  # { if (!is.na(fit_all$p50)) geom_vline(xintercept = fit_all$p50, linetype = "dashed", color = "grey50") } +
  geom_line(data = data.frame(x = x_range, y = overall_pred), aes(x, y),
            color = "black", linewidth = 0.5) +
  geom_point(data = obs_all, aes(x, value), color = "black", size = 1) +
  x_scale + y_scale +
  labs(title = "", #subtitle = stats_txt,
       x = "Price (₦)", y = "Proportion of repondents purchasing \n at each price (%) ") +
  annotation_logticks(sides = "b", size = 0.3) +
  axis_theme +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 8.5, color = "grey30"),
        axis.title = element_text(size = 13),
        axis.text  = element_text(size = 11))
print(p_global)
ggsave(file.path(OUT_DIR, "demand_curve_global.png"), p_global, width = 6.5, height = 4.5, dpi = 300)

# -----------------------------------------------------------------------------
# 7. FIT SUMMARY
# -----------------------------------------------------------------------------
row_of <- function(var, f) if (is.null(f)) NULL else
  data.frame(Variable = var, Group = f$label, Q0 = f$Qo, Alpha = f$alpha,
             P50 = f$p50, k = f$k, R_squared = f$r_squared)
fit_summary <- bind_rows(
  row_of("All", fit_all),
  bind_rows(lapply(names(all_results), function(v) bind_rows(row_of(v, all_results[[v]]$g1),
                                                             row_of(v, all_results[[v]]$g0)))))
cat("\nFIT SUMMARY (k fixed at", K_GLOBAL, "for every curve)\n")
print(fit_summary, row.names = FALSE)
write_csv(fit_summary, file.path(OUT_DIR, "demand_fit_summary.csv"))
