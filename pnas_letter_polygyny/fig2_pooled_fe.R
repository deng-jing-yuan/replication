# ==============================================================================
# Figure 2: Pooled OLS with census FE — wife/husband ratio on unmarried men
#           by age group (15-19 through 45-49)
#
# y_it = beta * wh_ratio_it + gamma * X_it + alpha_c + e_it
# where c indexes census (country x year) and i indexes locality
# ==============================================================================

library(data.table)
library(ggplot2)

# ---- 1. Load data ----

d <- fread("data/locality_fig2.csv")
cat(sprintf("Loaded %d localities across %d censuses in %d countries\n",
    nrow(d), length(unique(d$census_id)), length(unique(d$country))))

# ---- 2. Pooled FE regressions ----

outcome_vars <- c("single_1519", "single_2024", "single_2529",
                   "single_3034", "single_3539", "single_4044", "single_4549")
age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
cvars <- c("avg_educ", "log_popdens", "sex_ratio")

results_list <- list()

for (i in seq_along(outcome_vars)) {
  yvar <- outcome_vars[i]
  alab <- age_labels[i]

  # Without controls
  d_biv <- d[!is.na(get(yvar)) & !is.na(wh_ratio)]
  if (nrow(d_biv) > 10 && length(unique(d_biv$census_id)) > 1) {
    fml <- as.formula(paste0(yvar, " ~ wh_ratio + factor(census_id)"))
    m <- lm(fml, data = d_biv)
    s <- summary(m)$coefficients["wh_ratio", ]
    results_list[[paste0(yvar, "_biv")]] <- data.table(
      age_bin = alab, spec = "Without controls",
      beta = s[1], se = s[2], pval = s[4],
      n_obs = nrow(d_biv), n_cens = length(unique(d_biv$census_id))
    )
  }

  # With controls
  allv <- c(yvar, "wh_ratio", cvars)
  d_ctrl <- d[complete.cases(d[, ..allv])]
  if (nrow(d_ctrl) > 10 && length(unique(d_ctrl$census_id)) > 1) {
    fml <- as.formula(paste0(yvar, " ~ wh_ratio + ",
                             paste(cvars, collapse = " + "),
                             " + factor(census_id)"))
    m <- lm(fml, data = d_ctrl)
    s <- summary(m)$coefficients["wh_ratio", ]
    results_list[[paste0(yvar, "_ctrl")]] <- data.table(
      age_bin = alab, spec = "With controls",
      beta = s[1], se = s[2], pval = s[4],
      n_obs = nrow(d_ctrl), n_cens = length(unique(d_ctrl$census_id))
    )
  }

  cat(sprintf("  %s: done\n", alab))
}

res <- rbindlist(results_list)
res[, ci_lo := beta - 1.96 * se]
res[, ci_hi := beta + 1.96 * se]
res[, sig := fifelse(pval < 0.05, "p < 0.05", "n.s.")]

# ---- 3. Print results ----

cat("\n========================================\n")
cat("        POOLED FE RESULTS\n")
cat("========================================\n\n")
print(res[, .(age_bin, spec, beta = round(beta, 4),
              se = round(se, 4), pval = round(pval, 4),
              n_obs, n_cens)])

# ---- 4. Plot ----

res[, age_bin := factor(age_bin, levels = age_labels)]
res[, spec := factor(spec, levels = c("Without controls", "With controls"))]

p <- ggplot(res, aes(x = age_bin, y = beta, colour = sig)) +
  facet_wrap(~ spec) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c("p < 0.05" = "#D62728", "n.s." = "grey55"),
    name = NULL) +
  labs(x = "Age group of men",
       y = "Coefficient on wife/husband ratio",
       title = "Association between wife/husband ratio and share of never-married men",
       subtitle = "Pooled OLS with census (country x year) fixed effects") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey30"),
    strip.text    = element_text(face = "bold", size = 11),
    legend.position = "bottom")

ggsave(file.path("output", "fig2_pooled_fe.png"), p,
       width = 10, height = 5, dpi = 300)

cat("\n-> output/fig2_pooled_fe.png\n")
cat("Done.\n")
