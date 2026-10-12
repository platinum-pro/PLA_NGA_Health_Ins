# =============================================================================
# HEALTH INSURANCE PURCHASE TASK (HIPT): FINAL ANALYSIS RUN
#
# Run this once. Every number in the manuscript should come from this run.
#
# Settings that define the final run (section 0):
#   DATA_PATH          updated file with Economy recovered for six participants
#   EXCLUDE_UNDER18    TRUE, removes the participant aged 17 from all analyses
#   COVARIATE_MIN_RESP 1, covariate models draw from the primary sample
#   INTERACTION_TEST   "LRT", the single test used for all interaction p-values
#                      and for Holm adjustment. Chosen before seeing final output.
#
# New in this version:
#   F1  Interaction p-values and Holm adjustment use INTERACTION_TEST.
#   F2  Completion-by-condition results are stored for the numbers summary.
#   F3  Breakpoints are reported with and without the six recovered participants.
#   F4  A before/after comparison against the earlier 73-person primary result.
#   F5  Section 11 writes manuscript_numbers.txt with the key figures.
#   F6  Checks that no participant with a price response lacks a condition.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(lme4)
  library(broom.mixed)
})

# -----------------------------------------------------------------------------
# 0. SETTINGS
# -----------------------------------------------------------------------------
DATA_PATH <- Sys.getenv("HIPT_DATA", "~/Downloads/Health_Insurance_recoded - 28_Sep_2026.csv")
OUT_DIR   <- "hipt_outputs"
dir.create(OUT_DIR, showWarnings = FALSE)

EXCLUDE_UNDER18    <- TRUE
COVARIATE_MIN_RESP <- 1
INTERACTION_TEST   <- "LRT"        # "LRT" or "Wald"
stopifnot(INTERACTION_TEST %in% c("LRT", "Wald"))

# Labels for the 0/1 codes. VERIFY each against the Qualtrics export.
# GENDER: the data show code 0 = 22 and code 1 = 29, while the manuscript
# reports 29 men and 22 women. One of the two is reversed.
ECONOMY_LABELS <- c("Closed", "Open")
GENDER_LABELS  <- c("Female", "Male")   # code 0 = Female, code 1 = Male
INC_LABELS     <- c("Low", "High")
HINS_LABELS    <- c("No", "Yes")
YN_LABELS      <- c("No", "Yes")

price_cols <- c("P5", "P25", "P65", "P125", "P250", "P500", "P1500",
                "P3000", "P5500", "P17500", "P35000", "P70000",
                "P140000", "P280000", "P560000")
price_values <- c(5, 25, 65, 125, 250, 500, 1500, 3000, 5500,
                  17500, 35000, 70000, 140000, 280000, 560000)
CORRIDOR <- c(3000, 17500)

# The six participants whose condition was recovered from raw Qualtrics data
RECOVERED_IDS <- c("R_223ay6keI1QrTsO", "R_2Sxi0Fnfw8zdYDO", "R_2V69Gh7TZzBU4al",
                   "R_82zZceyWMFsktcB", "R_84s6YV9nQ3Zm5d7", "R_8ZRkP0HSRkl7PR7")

# Earlier primary result (N = 73, before recovery and age exclusion), for comparison
OLD_REF <- list(n = 73, beta = -0.5501, p_wald = 0.2564, price_beta = -1.6113)

banner <- function(txt) cat("\n", strrep("=", 78), "\n", txt, "\n",
                            strrep("=", 78), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 1. LOAD AND CHECK
# -----------------------------------------------------------------------------
df <- read_csv(DATA_PATH, show_col_types = FALSE)
stopifnot(!anyDuplicated(df$ResponseId))
banner("1. DATA CHECKS")
n_initiated <- nrow(df)
cat("Records in file (initiated):", n_initiated, "\n\n")

for (v in c("Economy", "GENDER", "INC", "HINS", "ALC", "MJ", "GB", "NIC")) {
  cat(v, ":\n"); print(table(df[[v]], useNA = "ifany")); cat("\n")
}
cat("AGE:\n");      print(summary(df$AGE))
cat("RELIGION:\n"); print(summary(df$RELIGION))

under18 <- df %>% filter(!is.na(AGE), AGE < 18) %>% select(ResponseId, AGE)
cat("\nParticipants with AGE < 18:", nrow(under18), "\n")
if (nrow(under18) > 0) print(under18)
n_excluded_age <- 0L
if (EXCLUDE_UNDER18 && nrow(under18) > 0) {
  df <- df %>% filter(!ResponseId %in% under18$ResponseId)
  n_excluded_age <- nrow(under18)
  cat("EXCLUDE_UNDER18 = TRUE: removed", n_excluded_age, "record(s). Eligible N =", nrow(df), "\n")
}
n_eligible <- nrow(df)

# -----------------------------------------------------------------------------
# 2. PARTICIPANT-LEVEL FLAGS AND RESPONSE PATTERNS
# -----------------------------------------------------------------------------
resp_mat <- as.matrix(df[, price_cols])

pt <- df %>%
  mutate(
    n_resp   = rowSums(!is.na(resp_mat)),
    any_resp = n_resp >= 1,
    ge3      = n_resp >= 3,
    complete = n_resp == length(price_cols),
    has_econ = Economy %in% c(0, 1)
  )

assess_pattern <- function(r) {
  out <- list(pattern_type = NA_character_, systematic = NA,
              n_reversals = NA_integer_, n_down = NA_integer_,
              bp_eligible = FALSE, bp_mid = NA_real_, bp_last_yes = NA_real_,
              bp_first_no = NA_real_, bp_mid_gap = NA_real_)
  obs_idx <- which(!is.na(r))
  if (length(obs_idx) < 3) {
    out$pattern_type <- "insufficient_data"
    return(as_tibble(out))
  }
  obs <- r[obs_idx]
  if (all(obs == 1)) {
    out[c("pattern_type", "systematic", "n_reversals", "n_down")] <-
      list("all_yes", TRUE, 0L, 0L)
    return(as_tibble(out))
  }
  if (all(obs == 0)) {
    out[c("pattern_type", "systematic", "n_reversals", "n_down")] <-
      list("all_no", TRUE, 0L, 0L)
    return(as_tibble(out))
  }
  a <- r[-length(r)]; b <- r[-1]
  ok <- !is.na(a) & !is.na(b)
  d  <- (b - a)[ok]
  out$n_reversals <- as.integer(sum(d > 0))
  out$n_down      <- as.integer(sum(d < 0))
  out$systematic  <- out$n_reversals <= 2
  if (!out$systematic) {
    out$pattern_type <- "non_systematic"
    return(as_tibble(out))
  }
  j <- which(obs[-length(obs)] == 1 & obs[-1] == 0)
  if (length(j) > 0) {
    jj <- max(j)
    out$bp_mid_gap <- sqrt(price_values[obs_idx[jj]] * price_values[obs_idx[jj + 1]])
  }
  k <- which(ok & a == 1 & b == 0)
  if (length(k) > 0) {
    i <- max(k)
    out$bp_eligible <- TRUE
    out$bp_mid      <- sqrt(price_values[i] * price_values[i + 1])
    out$bp_last_yes <- price_values[i]
    out$bp_first_no <- price_values[i + 1]
    out$pattern_type <- if (out$n_reversals == 0) "perfectly_monotonic" else "near_monotonic"
  } else {
    out$pattern_type <- "no_adjacent_yes_to_no"
  }
  as_tibble(out)
}

pattern_tbl <- bind_rows(lapply(seq_len(nrow(pt)), function(i) assess_pattern(resp_mat[i, ])))
pt <- bind_cols(pt, pattern_tbl)

# -----------------------------------------------------------------------------
# 3. MISSING DATA AND COMPLETION
# -----------------------------------------------------------------------------
banner("3. MISSING DATA")

cat("Eligible records:", nrow(pt), "\n")
cat("No price responses:", sum(!pt$any_resp), "\n")
cat(">=1 response:", sum(pt$any_resp), "\n")
cat(">=3 responses:", sum(pt$ge3), sprintf("(%.1f%% of eligible)\n", 100 * mean(pt$ge3)))
cat("All 15 responses:", sum(pt$complete), "\n")
cat("No Economy value:", sum(!pt$has_econ),
    "| of these with no price responses:", sum(!pt$has_econ & !pt$any_resp), "\n")
n_resp_no_econ <- sum(pt$any_resp & !pt$has_econ)
cat("Responded but Economy missing:", n_resp_no_econ, "\n")
if (n_resp_no_econ > 0) {
  warning("Some participants with price responses still have no Economy value.")   # F6
}

miss_by_price <- tibble(
  price = price_values,
  n_missing = colSums(is.na(pt[, price_cols])),
  prop_missing = colMeans(is.na(pt[, price_cols]))
)
print(miss_by_price, n = Inf)
sp <- suppressWarnings(cor.test(seq_along(price_values), miss_by_price$prop_missing,
                                method = "spearman", exact = FALSE))
cat(sprintf("\nSpearman (price rank vs proportion missing): rho = %.3f, p = %.4f\n",
            sp$estimate, sp$p.value))

comp_test <- function(flag, label) {   # F2
  d <- pt %>% filter(has_econ)
  tab <- table(Economy = factor(d$Economy, 0:1, ECONOMY_LABELS),
               Complete = factor(d[[flag]], c(FALSE, TRUE), c("No", "Yes")))
  cat("\n", label, " (denominator: participants assigned to a condition)\n", sep = "")
  print(tab); print(round(prop.table(tab, 1), 3))
  ch <- suppressWarnings(chisq.test(tab, correct = FALSE))
  fi <- fisher.test(tab)
  cat(sprintf("Chi-square = %.2f, p = %.4f | Fisher exact p = %.4f\n",
              ch$statistic, ch$p.value, fi$p.value))
  tibble(threshold = label,
         closed_yes = tab[1, 2], closed_n = sum(tab[1, ]),
         open_yes = tab[2, 2], open_n = sum(tab[2, ]),
         chisq_p = ch$p.value, fisher_p = fi$p.value)
}
comp1 <- comp_test("any_resp", ">=1 response")
comp3 <- comp_test("ge3",      ">=3 responses")
write_csv(bind_rows(comp1, comp3), file.path(OUT_DIR, "completion_by_condition.csv"))

cat("\nAge and religiosity, >=3 responses versus fewer:\n")
for (v in c("AGE", "RELIGION")) {
  x <- pt[[v]][pt$ge3];  y <- pt[[v]][!pt$ge3]
  x <- x[!is.na(x)];     y <- y[!is.na(y)]
  if (length(x) > 1 && length(y) > 1) {
    tt <- t.test(x, y)
    cat(sprintf("%s: t(%.0f) = %.2f, p = %.3f | >=3: M = %.2f (n = %d); <3: M = %.2f (n = %d)\n",
                v, tt$parameter, tt$statistic, tt$p.value, mean(x), length(x), mean(y), length(y)))
  }
}

cat("\nEarly purchase rate (first five prices), complete versus partial responders:\n")
early <- resp_mat[, 1:5]
pt$n_early   <- rowSums(!is.na(early))
pt$early_yes <- ifelse(pt$n_early >= 3, rowSums(early == 1, na.rm = TRUE) / pt$n_early, NA)
e_c <- pt$early_yes[pt$complete   & !is.na(pt$early_yes)]
e_p <- pt$early_yes[!pt$complete  & !is.na(pt$early_yes)]
if (length(e_c) > 1 && length(e_p) > 1) {
  tt <- t.test(e_c, e_p)
  cat(sprintf("Complete: M = %.1f%% (n = %d); partial: M = %.1f%% (n = %d); t(%.0f) = %.2f, p = %.3f\n",
              100 * mean(e_c), length(e_c), 100 * mean(e_p), length(e_p),
              tt$parameter, tt$statistic, tt$p.value))
}

# -----------------------------------------------------------------------------
# 4. LONG DATA AND ANALYSIS SETS
# -----------------------------------------------------------------------------
banner("4. ANALYSIS DATASETS")

log_price_center <- mean(log(price_values))
cat(sprintf("Model intercepts refer to the geometric mean price: N%.0f\n\n",
            exp(log_price_center)))

df_long <- pt %>%
  select(ResponseId, Economy, AGE, GENDER, INC, HINS, ALC, MJ, GB, NIC,
         RELIGION, all_of(price_cols)) %>%
  pivot_longer(all_of(price_cols), names_to = "price_label", values_to = "purchase") %>%
  mutate(price = as.numeric(str_remove(price_label, "^P")),
         log_price_c = log(price) - log_price_center)

cov_ok <- pt$any_resp & pt$has_econ & pt$n_resp >= COVARIATE_MIN_RESP
ids1     <- pt %>% filter(any_resp, has_econ) %>% pull(ResponseId)
ids1_ge3 <- pt %>% filter(ge3, has_econ) %>% pull(ResponseId)
ids2 <- pt$ResponseId[cov_ok & !is.na(pt$AGE) & pt$GENDER %in% c(0, 1) & !is.na(pt$RELIGION)]
ids3 <- pt$ResponseId[cov_ok & pt$INC %in% c(0, 1) & pt$HINS %in% c(0, 1)]
ids4 <- pt$ResponseId[cov_ok & pt$ALC %in% c(0, 1) & pt$MJ %in% c(0, 1) &
                        pt$GB %in% c(0, 1) & pt$NIC %in% c(0, 1)]
cat("COVARIATE_MIN_RESP =", COVARIATE_MIN_RESP, "\n")

build <- function(ids, z_vars = character()) {
  d <- df_long %>% filter(ResponseId %in% ids, !is.na(purchase))
  if (length(z_vars) > 0) {
    p <- pt %>% filter(ResponseId %in% ids) %>% select(ResponseId, all_of(z_vars))
    for (v in z_vars) p[[paste0(v, "_z")]] <- as.numeric(scale(p[[v]]))
    p <- p %>% select(ResponseId, ends_with("_z"))
    d <- left_join(d, p, by = "ResponseId")
  }
  d %>% mutate(
    Economy_f = factor(Economy, 0:1, ECONOMY_LABELS),
    GENDER_f  = factor(GENDER,  0:1, GENDER_LABELS),
    INC_f     = factor(INC,     0:1, INC_LABELS),
    HINS_f    = factor(HINS,    0:1, HINS_LABELS),
    ALC_f = factor(ALC, 0:1, YN_LABELS), MJ_f = factor(MJ, 0:1, YN_LABELS),
    GB_f  = factor(GB,  0:1, YN_LABELS), NIC_f = factor(NIC, 0:1, YN_LABELS)
  )
}

data1     <- build(ids1)
data1_ge3 <- build(ids1_ge3)
data2     <- build(ids2, c("AGE", "RELIGION"))
data3     <- build(ids3)
data4     <- build(ids4)

stage_row <- function(label, ids) {
  e <- pt$Economy[match(ids, pt$ResponseId)]
  tibble(stage = label, n = length(ids),
         closed = sum(e == 0, na.rm = TRUE), open = sum(e == 1, na.rm = TRUE),
         economy_missing = sum(is.na(e)))
}
sys_ids <- pt %>% filter(ge3, systematic %in% TRUE) %>% pull(ResponseId)
bp_ids  <- pt %>% filter(bp_eligible) %>% pull(ResponseId)
ge3_ids <- pt$ResponseId[pt$ge3]

flow <- bind_rows(
  stage_row("Eligible records", pt$ResponseId),
  stage_row(">=1 price response", pt$ResponseId[pt$any_resp]),
  stage_row("Primary sample: >=1 response and Economy assigned", ids1),
  stage_row(">=3 price responses (all)", ge3_ids),
  stage_row(">=3 price responses and Economy assigned", ids1_ge3),
  stage_row("Systematic responders (of >=3)", sys_ids),
  stage_row("Breakpoint eligible", bp_ids),
  stage_row("Demographics model (combined)", ids2),
  stage_row("Socioeconomic model (combined)", ids3),
  stage_row("Substance use model (combined)", ids4)
) %>%
  mutate(with_ge3 = c(rep(NA, 7), sum(ids2 %in% ge3_ids), sum(ids3 %in% ge3_ids),
                      sum(ids4 %in% ge3_ids)))
cat("PARTICIPANT FLOW (with_ge3 = how many of the model sample have >=3 responses)\n")
print(flow, n = Inf, width = Inf)
write_csv(flow, file.path(OUT_DIR, "participant_flow.csv"))

cat("\nSystematicity pattern counts (eligible participants):\n")
print(count(pt, pattern_type) %>% mutate(pct = round(100 * n / sum(n), 1)))

# -----------------------------------------------------------------------------
# 5. TABLE 1 FOR THREE SAMPLES
# -----------------------------------------------------------------------------
banner("5. TABLE 1")

cat_rows <- function(p, var, labels, title, miss_label = "Did not respond") {
  x <- p[[var]]
  lev <- c(labels, miss_label)
  lab <- ifelse(is.na(x), miss_label, labels[match(x, 0:1)])
  lab[is.na(lab)] <- miss_label
  tab <- table(factor(lab, levels = lev))
  tibble(variable = title, level = lev,
         value = sprintf("%d (%.1f%%)", as.integer(tab), 100 * as.integer(tab) / nrow(p)))
}
num_rows <- function(p, var, title) {
  x <- p[[var]]
  tibble(variable = title, level = c("M (SD)", "Missing, n"),
         value = c(sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
                   as.character(sum(is.na(x)))))
}
table1_col <- function(p) {
  bind_rows(
    num_rows(p, "AGE", "Age"),
    cat_rows(p, "GENDER", GENDER_LABELS, "Gender"),
    num_rows(p, "RELIGION", "Religiosity (0-100)"),
    cat_rows(p, "INC", INC_LABELS, "Income (binary)"),
    cat_rows(p, "HINS", HINS_LABELS, "Has health insurance"),
    cat_rows(p, "Economy", ECONOMY_LABELS, "Economy condition", "No condition assigned")
  )
}
t1 <- table1_col(pt) %>% rename(`Eligible` = value) %>%
  left_join(table1_col(filter(pt, ge3)) %>% rename(`>=3 responses` = value),
            by = c("variable", "level")) %>%
  left_join(table1_col(filter(pt, ResponseId %in% ids1)) %>% rename(`Primary sample` = value),
            by = c("variable", "level"))
names(t1)[3:5] <- c(sprintf("Eligible (N = %d)", nrow(pt)),
                    sprintf(">=3 responses (n = %d)", sum(pt$ge3)),
                    sprintf("Primary sample (n = %d)", length(ids1)))
print(t1, n = Inf, width = Inf)
write_csv(t1, file.path(OUT_DIR, "table1_three_samples.csv"))
cat("\nRebuild the four-level income rows and the insurance rows from the raw\n",
    "Qualtrics items before finalizing Table 1.\n", sep = "")

# -----------------------------------------------------------------------------
# 6. MODEL HELPERS
# -----------------------------------------------------------------------------
fit_log <- tibble(label = character(), optimizer = character(),
                  clean = logical(), singular = logical())

fit_rs <- function(f, d, label = "") {
  optimizers <- list(
    bobyqa    = glmerControl(optimizer = "bobyqa",    optCtrl = list(maxfun = 2e5)),
    nloptwrap = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 2e5)),
    NM        = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 2e5)))
  best <- NULL; best_clean <- FALSE; best_nm <- NA_character_
  for (nm in names(optimizers)) {
    warn <- character()
    m <- tryCatch(withCallingHandlers(
      glmer(f, data = d, family = binomial(link = "logit"), control = optimizers[[nm]]),
      warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) NULL)
    if (is.null(m)) next
    clean <- length(warn) == 0 && is.null(m@optinfo$conv$lme4$messages)
    if (is.null(best)) { best <- m; best_clean <- clean; best_nm <- nm }
    if (clean) { best <- m; best_clean <- TRUE; best_nm <- nm; break }
  }
  if (is.null(best)) {
    message("FIT FAILED: ", label)
    fit_log <<- bind_rows(fit_log, tibble(label = label, optimizer = NA_character_,
                                          clean = FALSE, singular = NA))
    return(NULL)
  }
  sing <- isSingular(best, tol = 1e-4)
  attr(best, "fit_clean") <- best_clean
  message(label, ": optimizer = ", best_nm, ", clean convergence = ", best_clean,
          if (sing) ", SINGULAR" else "")
  fit_log <<- bind_rows(fit_log, tibble(label = label, optimizer = best_nm,
                                        clean = best_clean, singular = sing))
  best
}
is_clean <- function(m) !is.null(m) && isTRUE(attr(m, "fit_clean"))

re_summary <- function(m) {
  vc <- as.data.frame(VarCorr(m))
  tibble(sd_intercept = vc$sdcor[vc$var1 == "(Intercept)" & is.na(vc$var2)],
         sd_slope     = vc$sdcor[vc$var1 == "log_price_c" & is.na(vc$var2)],
         corr         = vc$sdcor[!is.na(vc$var2)],
         singular     = isSingular(m, tol = 1e-4), AIC = AIC(m))
}

fixed_tbl <- function(m, label) {
  tidy(m, effects = "fixed", conf.int = TRUE) %>%
    transmute(model = label, term, beta = estimate, ci_low = conf.low,
              ci_high = conf.high, p = p.value, clean_convergence = is_clean(m))
}

test_interaction <- function(main_model, int_f, d, var_pat, label) {
  m1  <- fit_rs(int_f, d, paste(label, "interaction"))
  lrt <- anova(main_model, m1)
  co  <- tidy(m1, effects = "fixed", conf.int = TRUE) %>%
    filter(grepl("^log_price_c:", term), grepl(var_pat, term, fixed = TRUE))
  p_w <- co$p.value; p_l <- lrt$`Pr(>Chisq)`[2]
  tibble(test = label, n = n_distinct(d$ResponseId),
         beta = co$estimate, ci_low = co$conf.low, ci_high = co$conf.high,
         p_wald = p_w, chisq = lrt$Chisq[2], p_lrt = p_l,
         p_report = if (INTERACTION_TEST == "LRT") p_l else p_w,        # F1
         clean = is_clean(main_model) && is_clean(m1))
}
add_holm <- function(x) x %>% mutate(p_report_holm = p.adjust(p_report, method = "holm"))

re <- "(1 + log_price_c | ResponseId)"
fm <- function(rhs) as.formula(paste("purchase ~", rhs, "+", re))

# -----------------------------------------------------------------------------
# 7. PRIMARY ANALYSIS: ECONOMY
# -----------------------------------------------------------------------------
banner("7. MODEL 1: ECONOMY MANIPULATION")

f1 <- purchase ~ log_price_c * Economy_f + (1 + log_price_c | ResponseId)

m1_primary <- fit_rs(f1, data1, "Model 1 primary (>=1 response)")
cat("PRIMARY: participants with >=1 response and Economy assigned\n")
cat("N =", n_distinct(data1$ResponseId), "participants;", nrow(data1), "observations\n")
print(summary(m1_primary))
print(re_summary(m1_primary))

m1_ri <- fit_rs(purchase ~ log_price_c * Economy_f + (1 | ResponseId), data1,
                "Model 1 random intercept only")
cat("\nRandom slope versus random intercept only:\n"); print(anova(m1_ri, m1_primary))

m1_ge3 <- fit_rs(f1, data1_ge3, "Model 1 sensitivity A (>=3 responses)")
cat("\nSENSITIVITY A: >=3 responses, N =", n_distinct(data1_ge3$ResponseId), "\n")
print(fixed_tbl(m1_ge3, "sens_A_ge3"), n = Inf); print(re_summary(m1_ge3))

cat("\nParticipants in the primary sample with <3 responses:",
    length(ids1) - length(ids1_ge3), "\n")
zero_var_ge3 <- data1_ge3 %>% group_by(ResponseId) %>%
  summarise(v = n_distinct(purchase), .groups = "drop") %>% filter(v == 1) %>% pull(ResponseId)
cat("Among the >=3 responders, all-yes or all-no:", length(zero_var_ge3), "\n")
data1_ge3_var <- filter(data1_ge3, !ResponseId %in% zero_var_ge3)
m1_var <- fit_rs(f1, data1_ge3_var, "Model 1 sensitivity B (>=3 responses, varying)")
cat("SENSITIVITY B: >=3 responses and excluding all-yes/all-no, N =",
    n_distinct(data1_ge3_var$ResponseId), "\n")
print(fixed_tbl(m1_var, "sens_B_ge3_varying"), n = Inf); print(re_summary(m1_var))

m1_null <- fit_rs(purchase ~ log_price_c + Economy_f + (1 + log_price_c | ResponseId),
                  data1, "Model 1 no interaction")
econ_test <- test_interaction(m1_null, f1, data1, "Economy_f", "Economy x Price (primary)")
print(econ_test, width = Inf)

# F4: before and after comparison with the earlier 73-person result
co1 <- tidy(m1_primary, effects = "fixed", conf.int = TRUE)
new_int <- co1[co1$term == "log_price_c:Economy_fOpen", ]
new_price <- co1[co1$term == "log_price_c", ]
cat("\nBEFORE / AFTER (primary model)\n")
cat(sprintf("  Earlier (N = %d): price beta = %.2f; Economy x Price beta = %.2f, Wald p = %.3f\n",
            OLD_REF$n, OLD_REF$price_beta, OLD_REF$beta, OLD_REF$p_wald))
cat(sprintf("  Final   (N = %d): price beta = %.2f; Economy x Price beta = %.2f, Wald p = %.3f\n",
            n_distinct(data1$ResponseId), new_price$estimate, new_int$estimate, new_int$p.value))
cat("If these differ only slightly, the recovery and age exclusion did not change\n",
    "the conclusion. If they differ noticeably, describe the change in the response letter.\n", sep = "")

cat(sprintf("\nIntercept is the log-odds of purchase at the geometric mean price (about N%.0f)\n",
            exp(log_price_center)))
cat("for a participant with random effects of zero. Suggested wording:\n",
    "'overall purchase propensity at the geometric mean price'.\n", sep = "")

# -----------------------------------------------------------------------------
# 8. COMBINED EXPLORATORY MODELS (comparison only)
# -----------------------------------------------------------------------------
banner("8. COMBINED EXPLORATORY MODELS")

main2 <- fm("log_price_c + Economy_f + AGE_z + GENDER_f + RELIGION_z")
m2 <- fit_rs(main2, data2, "Model 2 demographics (combined)")
main3 <- fm("log_price_c + Economy_f + INC_f + HINS_f")
m3 <- fit_rs(main3, data3, "Model 3 socioeconomic (combined)")
main4 <- fm("log_price_c + Economy_f + ALC_f + MJ_f + GB_f + NIC_f")
m4 <- fit_rs(main4, data4, "Model 4 substance use (combined)")

fixed_all <- bind_rows(
  fixed_tbl(m1_primary, "Model 1 (primary)"),
  fixed_tbl(m2, "Model 2 (demographics)"),
  fixed_tbl(m3, "Model 3 (socioeconomic)"),
  fixed_tbl(m4, "Model 4 (substance use)")
)
print(fixed_all, n = Inf)
write_csv(fixed_all, file.path(OUT_DIR, "main_effects_models.csv"))
if (!is_clean(m2)) {
  cat("\nWARNING: the combined demographic model (Model 2) did not converge cleanly.\n",
      "Do not report its coefficients or its interaction tests.\n", sep = "")
}

int_combined <- bind_rows(
  test_interaction(m2, fm("log_price_c * GENDER_f + Economy_f + AGE_z + RELIGION_z"),
                   data2, "GENDER_f", "Gender x Price"),
  test_interaction(m2, fm("log_price_c * AGE_z + Economy_f + GENDER_f + RELIGION_z"),
                   data2, "AGE_z", "Age x Price"),
  test_interaction(m2, fm("log_price_c * RELIGION_z + Economy_f + AGE_z + GENDER_f"),
                   data2, "RELIGION_z", "Religiosity x Price"),
  test_interaction(m3, fm("log_price_c * INC_f + Economy_f + HINS_f"),
                   data3, "INC_f", "Income x Price"),
  test_interaction(m3, fm("log_price_c * HINS_f + Economy_f + INC_f"),
                   data3, "HINS_f", "Insurance x Price"),
  test_interaction(m4, fm("log_price_c * ALC_f + Economy_f + MJ_f + GB_f + NIC_f"),
                   data4, "ALC_f", "Alcohol x Price"),
  test_interaction(m4, fm("log_price_c * GB_f + Economy_f + ALC_f + MJ_f + NIC_f"),
                   data4, "GB_f", "Gambling x Price")
) %>% add_holm()

cat("\nCOMBINED-MODEL INTERACTIONS (rows with clean = FALSE are not reportable)\n")
print(int_combined, n = Inf, width = Inf)
write_csv(int_combined, file.path(OUT_DIR, "exploratory_interactions_combined.csv"))

# -----------------------------------------------------------------------------
# 9. SINGLE-MODERATOR MODELS (supplement)
# -----------------------------------------------------------------------------
banner("9. SINGLE-MODERATOR MODELS")

specs <- list(
  list(label = "Age x Price",         var = "AGE",      type = "cont", term = "AGE_z"),
  list(label = "Gender x Price",      var = "GENDER",   type = "bin",  term = "GENDER_f"),
  list(label = "Religiosity x Price", var = "RELIGION", type = "cont", term = "RELIGION_z"),
  list(label = "Income x Price",      var = "INC",      type = "bin",  term = "INC_f"),
  list(label = "Insurance x Price",   var = "HINS",     type = "bin",  term = "HINS_f"),
  list(label = "Alcohol x Price",     var = "ALC",      type = "bin",  term = "ALC_f"),
  list(label = "Gambling x Price",    var = "GB",       type = "bin",  term = "GB_f")
)

run_single <- function(sp) {
  ok_var <- if (sp$type == "bin") pt[[sp$var]] %in% c(0, 1) else !is.na(pt[[sp$var]])
  ids <- pt$ResponseId[cov_ok & ok_var]
  d <- build(ids, if (sp$type == "cont") sp$var else character())
  m0 <- fit_rs(fm(paste("log_price_c + Economy_f +", sp$term)), d, paste(sp$label, "single main"))
  test_interaction(m0, fm(paste("log_price_c *", sp$term, "+ Economy_f")), d, sp$term, sp$label)
}
int_single <- bind_rows(lapply(specs, run_single)) %>% add_holm()

cat("SINGLE-MODERATOR INTERACTIONS (p_report uses the", INTERACTION_TEST,
    "test; Holm across the 7 tests)\n")
print(int_single, n = Inf, width = Inf)
write_csv(int_single, file.path(OUT_DIR, "exploratory_interactions_single.csv"))
cat("\nReport only rows with clean = TRUE. Treat all as exploratory.\n")

cat("\nFIT LOG\n")
print(fit_log, n = Inf, width = Inf)
write_csv(fit_log, file.path(OUT_DIR, "fit_log.csv"))

# -----------------------------------------------------------------------------
# 10. BREAKPOINTS
# -----------------------------------------------------------------------------
banner("10. BREAKPOINTS")

bp <- pt %>% filter(bp_eligible)
cat("Systematic responders among >=3:", sum(pt$systematic %in% TRUE),
    "| breakpoint eligible:", nrow(bp), "\n")
cat("Accounting: eligible + all_yes + all_no + no_adjacent_yes_to_no =",
    sum(pt$bp_eligible), "+", sum(pt$pattern_type == "all_yes"), "+",
    sum(pt$pattern_type == "all_no"), "+", sum(pt$pattern_type == "no_adjacent_yes_to_no"), "\n\n")

q <- quantile(bp$bp_mid, c(.25, .5, .75))
cat(sprintf("Midpoint (geometric) breakpoint: median N%.0f, IQR N%.0f to N%.0f, range N%.0f to N%.0f\n",
            q[2], q[1], q[3], min(bp$bp_mid), max(bp$bp_mid)))
cat(sprintf("Last-Yes median: N%.0f | First-No median: N%.0f\n",
            median(bp$bp_last_yes), median(bp$bp_first_no)))

gap <- pt %>% filter(!is.na(bp_mid_gap))
cat(sprintf("Gap-tolerant sensitivity (n = %d): median N%.0f\n", nrow(gap), median(gap$bp_mid_gap)))

below  <- sum(bp$bp_mid < CORRIDOR[1])
within <- sum(bp$bp_mid >= CORRIDOR[1] & bp$bp_mid <= CORRIDOR[2])
above  <- sum(bp$bp_mid > CORRIDOR[2])
cat(sprintf("\nRelative to the N3,000 to N17,500 corridor: below %d (%.1f%%), within %d (%.1f%%), above %d (%.1f%%)\n",
            below, 100 * below / nrow(bp), within, 100 * within / nrow(bp), above, 100 * above / nrow(bp)))

# F3: breakpoints by condition, with and without the six recovered participants
bp_lab <- bp %>%
  mutate(Economy_lab = ifelse(has_econ, as.character(factor(Economy, 0:1, ECONOMY_LABELS)),
                              "No condition assigned"),
         recovered = ResponseId %in% RECOVERED_IDS)
bp_by_econ <- bind_rows(
  bp_lab %>% group_by(Economy_lab) %>%
    summarise(set = "all breakpoint-eligible", n = n(), median_bp = median(bp_mid), .groups = "drop"),
  bp_lab %>% filter(!recovered) %>% group_by(Economy_lab) %>%
    summarise(set = "excluding the 6 recovered", n = n(), median_bp = median(bp_mid), .groups = "drop"))
cat("\nBreakpoints by condition (descriptive only):\n"); print(bp_by_econ, n = Inf)
cat("\nRecovered participants who are breakpoint eligible:\n")
print(bp_lab %>% filter(recovered) %>% select(ResponseId, Economy_lab, n_resp, bp_mid), n = Inf)

write_csv(pt %>% select(ResponseId, Economy, n_resp, ge3, pattern_type, n_reversals,
                        n_down, bp_eligible, bp_mid, bp_last_yes, bp_first_no, bp_mid_gap),
          file.path(OUT_DIR, "participant_patterns_breakpoints.csv"))

# -----------------------------------------------------------------------------
# 11. MANUSCRIPT NUMBERS  (F5)
# -----------------------------------------------------------------------------
banner("11. MANUSCRIPT NUMBERS")

fx <- function(m, term) { co <- tidy(m, effects = "fixed", conf.int = TRUE); co[co$term == term, ] }
ff <- function(x) formatC(x, format = "f", digits = 2)
fp <- function(p) ifelse(p < .001, "< .001", paste0("= ", sub("^0", "", formatC(p, format = "f", digits = 3))))

m1p <- fx(m1_primary, "log_price_c");            m1i <- fx(m1_primary, "log_price_c:Economy_fOpen")
m1e <- fx(m1_primary, "Economy_fOpen")
sa  <- fx(m1_ge3, "log_price_c:Economy_fOpen"); sb  <- fx(m1_var, "log_price_c:Economy_fOpen")
re1 <- re_summary(m1_primary)
fl  <- function(stage) flow[flow$stage == stage, ]
prim <- fl("Primary sample: >=1 response and Economy assigned")
g3   <- fl(">=3 price responses and Economy assigned")

lines <- c(
  "MANUSCRIPT NUMBERS (generated by the final run)",
  sprintf("Data file: %s", basename(DATA_PATH)),
  sprintf("EXCLUDE_UNDER18 = %s | COVARIATE_MIN_RESP = %d | INTERACTION_TEST = %s",
          EXCLUDE_UNDER18, COVARIATE_MIN_RESP, INTERACTION_TEST),
  "",
  "SAMPLE FLOW",
  sprintf("Records in file: %d; excluded for age under 18: %d; eligible: %d",
          n_initiated, n_excluded_age, n_eligible),
  sprintf("With >=1 price response: %d; with no price response: %d",
          sum(pt$any_resp), sum(!pt$any_resp)),
  sprintf("Primary sample (>=1 response and condition): %d (%d closed, %d open)",
          prim$n, prim$closed, prim$open),
  sprintf(">=3 responses: %d (%.1f%% of eligible); with condition: %d (%d closed, %d open)",
          sum(pt$ge3), 100 * mean(pt$ge3), g3$n, g3$closed, g3$open),
  sprintf("Systematic responders: %d; breakpoint eligible: %d",
          length(sys_ids), length(bp_ids)),
  sprintf("Condition recovered from raw Qualtrics records for %d participants", length(RECOVERED_IDS)),
  "",
  "COMPLETION BY CONDITION (>=3 responses, among assigned)",
  sprintf("Closed %d/%d (%.1f%%); open %d/%d (%.1f%%); chi-square p %s; Fisher p %s",
          comp3$closed_yes, comp3$closed_n, 100 * comp3$closed_yes / comp3$closed_n,
          comp3$open_yes, comp3$open_n, 100 * comp3$open_yes / comp3$open_n,
          fp(comp3$chisq_p), fp(comp3$fisher_p)),
  "",
  "PRIMARY MODEL (mixed-effects logistic, random intercept and slope for log price)",
  sprintf("Log price: beta = %s, 95%% CI [%s, %s], p %s",
          ff(m1p$estimate), ff(m1p$conf.low), ff(m1p$conf.high), fp(m1p$p.value)),
  sprintf("Economy: beta = %s, 95%% CI [%s, %s], p %s",
          ff(m1e$estimate), ff(m1e$conf.low), ff(m1e$conf.high), fp(m1e$p.value)),
  sprintf("Economy x log price: beta = %s, 95%% CI [%s, %s], Wald p %s; LRT p %s",
          ff(m1i$estimate), ff(m1i$conf.low), ff(m1i$conf.high), fp(m1i$p.value), fp(econ_test$p_lrt)),
  sprintf("Random-effect SDs: intercept %s, slope %s, correlation %s",
          ff(re1$sd_intercept), ff(re1$sd_slope), ff(re1$corr)),
  sprintf("Sensitivity A (>=3 responses, N = %d): Economy x price beta = %s, p %s",
          n_distinct(data1_ge3$ResponseId), ff(sa$estimate), fp(sa$p.value)),
  sprintf("Sensitivity B (>=3 responses, varying, N = %d): Economy x price beta = %s, p %s",
          n_distinct(data1_ge3_var$ResponseId), ff(sb$estimate), fp(sb$p.value)),
  "",
  "BREAKPOINTS",
  sprintf("n = %d; median N%s, IQR N%s to N%s",
          nrow(bp), format(round(q[2]), big.mark = ","),
          format(round(q[1]), big.mark = ","), format(round(q[3]), big.mark = ",")),
  sprintf("Below corridor %d (%.1f%%), within %d (%.1f%%), above %d (%.1f%%)",
          below, 100 * below / nrow(bp), within, 100 * within / nrow(bp),
          above, 100 * above / nrow(bp)),
  "",
  "MODEL CONVERGENCE",
  sprintf("Models in fit log: %d; clean convergence: %d; singular: %d",
          nrow(fit_log), sum(fit_log$clean, na.rm = TRUE), sum(fit_log$singular, na.rm = TRUE))
)
writeLines(lines, file.path(OUT_DIR, "manuscript_numbers.txt"))
cat(paste(lines, collapse = "\n"), "\n")

banner("DONE. All outputs are in the hipt_outputs folder.")



