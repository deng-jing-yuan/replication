# ==============================================================================
# Figure 1: Per-census forest plot of wife/husband ratio on unmarried men 20-29
#
# Runs OLS per census:  single_m20s ~ wh_ratio  (standardized)
# Two panels: without controls, with controls (educ, log popdens, sex ratio)
# ==============================================================================

library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)

# ---- 1. Load data ----

d <- fread("data/locality_fig1.csv")
cat(sprintf("Loaded %d localities across %d censuses in %d countries\n",
    nrow(d), length(unique(d$census_id)), length(unique(d$country))))

# ---- 2. Per-census OLS regressions ----

cvars <- c("avg_educ", "log_popdens", "sex_ratio")

run_ols <- function(d, yvar, xvar, controls = NULL) {
  results <- list()
  for (cid in sort(unique(d$census_id))) {
    dd <- d[census_id == cid]
    clabel <- dd$census_label[1]

    allv <- c(yvar, xvar, controls)
    dd_complete <- dd[complete.cases(dd[, ..allv])]
    if (nrow(dd_complete) < 10) next

    # Standardize within census
    dd_complete[, yvar_s := scale(get(yvar))]
    dd_complete[, xvar_s := scale(get(xvar))]
    if (!is.null(controls)) {
      for (cv in controls) dd_complete[, (paste0(cv, "_s")) := scale(get(cv))]
      fml <- as.formula(paste0("yvar_s ~ xvar_s + ",
                               paste0(controls, "_s", collapse = " + ")))
    } else {
      fml <- as.formula("yvar_s ~ xvar_s")
    }

    m <- lm(fml, data = dd_complete)
    s <- summary(m)$coefficients["xvar_s", ]
    results[[cid]] <- data.table(
      census_id = cid, census_label = clabel,
      beta = s[1], se = s[2], pval = s[4],
      n_loc = nrow(dd_complete)
    )
  }
  rbindlist(results)
}

cat("Running bivariate regressions...\n")
res_biv <- run_ols(d, "single_m20s", "wh_ratio")

cat("Running regressions with controls...\n")
res_ctrl <- run_ols(d, "single_m20s", "wh_ratio", cvars)

# ---- 3. Prepare plot data ----

res_biv[, ci_lo := beta - 1.96 * se]
res_biv[, ci_hi := beta + 1.96 * se]
res_ctrl[, ci_lo := beta - 1.96 * se]
res_ctrl[, ci_hi := beta + 1.96 * se]

make_sig <- function(pval, beta) {
  factor(ifelse(pval < 0.05 & beta > 0, "Positive (p < 0.05)",
         ifelse(pval < 0.05 & beta < 0, "Negative (p < 0.05)", "n.s.")),
         levels = c("Negative (p < 0.05)", "n.s.", "Positive (p < 0.05)"))
}

res_biv[, sig := make_sig(pval, beta)]
res_ctrl[, sig := make_sig(pval, beta)]

# Independent ordering per panel
res_biv[, census_label := factor(census_label,
          levels = res_biv[order(beta), census_label])]
res_ctrl[, census_label := factor(census_label,
           levels = res_ctrl[order(beta), census_label])]

# Clip CIs for display
xlim_range <- c(-1.5, 2.5)
res_biv[, ci_lo_clip := pmax(ci_lo, xlim_range[1])]
res_biv[, ci_hi_clip := pmin(ci_hi, xlim_range[2])]
res_ctrl[, ci_lo_clip := pmax(ci_lo, xlim_range[1])]
res_ctrl[, ci_hi_clip := pmin(ci_hi, xlim_range[2])]

# ---- 4. Print summary ----

cat(sprintf("\nWithout controls: %d censuses\n", nrow(res_biv)))
cat(sprintf("  Positive (p<0.05): %d\n", sum(res_biv$sig == "Positive (p < 0.05)")))
cat(sprintf("  Null:              %d\n", sum(res_biv$sig == "n.s.")))
cat(sprintf("  Negative (p<0.05): %d\n", sum(res_biv$sig == "Negative (p < 0.05)")))
cat(sprintf("  Median beta:       %.3f\n", median(res_biv$beta)))

cat(sprintf("\nWith controls: %d censuses\n", nrow(res_ctrl)))
cat(sprintf("  Positive (p<0.05): %d\n", sum(res_ctrl$sig == "Positive (p < 0.05)")))
cat(sprintf("  Null:              %d\n", sum(res_ctrl$sig == "n.s.")))
cat(sprintf("  Negative (p<0.05): %d\n", sum(res_ctrl$sig == "Negative (p < 0.05)")))
cat(sprintf("  Median beta:       %.3f\n", median(res_ctrl$beta)))

# ---- 5. Plot ----

sig_cols <- c("Positive (p < 0.05)" = "#D62728",
              "Negative (p < 0.05)" = "#1F77B4",
              "n.s." = "grey55")

forest_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, margin = margin(b = 4)),
    axis.text.y        = element_text(size = 7.5, margin = margin(r = 1)),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 10, margin = margin(t = 6)),
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(4, 10, 4, 4)
  )

p1 <- ggplot(res_biv, aes(x = beta, y = census_label, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo_clip, xmax = ci_hi_clip),
                 height = 0, linewidth = 0.35) +
  geom_point(size = 1.0, shape = 16) +
  scale_colour_manual(values = sig_cols, drop = FALSE) +
  scale_x_continuous(breaks = seq(-1.5, 2.5, 0.5)) +
  coord_cartesian(xlim = xlim_range) +
  labs(x = "Standardized coefficient", y = NULL, title = "Without controls") +
  forest_theme

p2 <- ggplot(res_ctrl, aes(x = beta, y = census_label, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo_clip, xmax = ci_hi_clip),
                 height = 0, linewidth = 0.35) +
  geom_point(size = 1.0, shape = 16) +
  scale_colour_manual(values = sig_cols, drop = FALSE) +
  scale_x_continuous(breaks = seq(-1.5, 2.5, 0.5)) +
  coord_cartesian(xlim = xlim_range) +
  labs(x = "Standardized coefficient", y = NULL, title = "With controls") +
  forest_theme

# Extract legend
p_leg <- ggplot(res_biv, aes(x = beta, y = census_label, colour = sig)) +
  geom_point() +
  scale_colour_manual(values = sig_cols, name = NULL, drop = FALSE) +
  theme(legend.position = "bottom", legend.text = element_text(size = 9))
g_tmp <- ggplotGrob(p_leg)
legend_idx <- which(sapply(g_tmp$grobs, function(g) !is.null(g$name) && grepl("guide", g$name)))
legend_grob <- g_tmp$grobs[[legend_idx[1]]]

title_grob <- textGrob(
  "Association between wife/husband ratio and share of never-married men (20\u201329)",
  gp = gpar(fontface = "bold", fontsize = 13))
subtitle_grob <- textGrob(
  "Per-census OLS across subnational localities",
  gp = gpar(fontsize = 10, col = "grey30"))

n_max <- max(nrow(res_biv), nrow(res_ctrl))
plot_h <- n_max * 0.155 + 2.2

png(file.path("output", "fig1_forest.png"),
    width = 10, height = plot_h, units = "in", res = 300)
grid.arrange(
  title_grob, subtitle_grob,
  arrangeGrob(p1, p2, ncol = 2),
  legend_grob,
  ncol = 1,
  heights = unit(c(0.5, 0.35, plot_h - 1.65, 0.4), "in")
)
dev.off()

cat(sprintf("\n-> output/fig1_forest.png\n"))
cat("Done.\n")
