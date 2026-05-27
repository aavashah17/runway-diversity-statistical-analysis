# ═══════════════════════════════════════════════════════════════════════════════
# Fashion Week Diversity: Has Progress Been Real — and Equal Across Cities?
# ANOVA Analysis of POC Model Representation (2015–2023)
#
# Data Source: The Fashion Spot Annual Runway Diversity Reports
# Method:      ANOVA with Bonferroni-corrected pairwise t-tests
# Author:      Aava


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ── 0. Setup ──────────────────────────────────────────────────────────────────

set.seed(42)
setwd("~/Desktop/stat 311")

# City color palette — fashion-forward, not default R colors
city_colors <- c(
  "New York" = "#1a1a2e",   # deep navy
  "London"   = "#c84b31",   # burnt red
  "Milan"    = "#ecdbba",   # warm cream (outlined)
  "Paris"    = "#6b4c9a"    # deep violet
)

city_colors_solid <- c(
  "New York" = "#1a1a2e",
  "London"   = "#c84b31",
  "Milan"    = "#c8a96e",   # darker for visibility
  "Paris"    = "#6b4c9a"
)

# ── 1. Load & Inspect Data ────────────────────────────────────────────────────

df <- read.csv("fashion_diversity.csv", stringsAsFactors = FALSE)
df$city <- factor(df$city, levels = c("New York", "London", "Paris", "Milan"))

cat("═══════════════════════════════════════════════════\n")
cat("  FASHION WEEK DIVERSITY ANALYSIS — DATA SUMMARY\n")
cat("═══════════════════════════════════════════════════\n\n")
cat("Dataset dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
cat("Years covered:", min(df$year), "–", max(df$year), "\n")
cat("Cities:", paste(levels(df$city), collapse=", "), "\n\n")

# Summary statistics by city
summary_stats <- df %>%
  group_by(city) %>%
  summarise(
    n         = n(),
    mean_poc  = round(mean(poc_pct), 2),
    sd_poc    = round(sd(poc_pct), 2),
    min_poc   = round(min(poc_pct), 2),
    max_poc   = round(max(poc_pct), 2),
    range_poc = round(max(poc_pct) - min(poc_pct), 2),
    .groups   = "drop"
  )

cat("── Descriptive Statistics by City ──\n")
print(summary_stats, n = Inf)
cat("\n")

# ── 2. ANOVA Assumption Checks ────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════\n")
cat("  ASSUMPTION CHECKS\n")
cat("═══════════════════════════════════════════════════\n\n")

# ── Assumption 1: Independence
# Each observation is a distinct fashion week season in a distinct city.
# Seasons do not overlap; cities are independent fashion markets.
# Assumption is met by study design.
cat("1. INDEPENDENCE\n")
cat("   Each observation = one city × one year (n=9 per city, 36 total).\n")
cat("   Cities are independent fashion markets. ✓\n\n")

# ── Assumption 2: Approximate Normality (Shapiro-Wilk per group)
cat("2. NORMALITY (Shapiro-Wilk test per city)\n")
shapiro_results <- df %>%
  group_by(city) %>%
  summarise(
    W       = round(shapiro.test(poc_pct)$statistic, 4),
    p_value = round(shapiro.test(poc_pct)$p.value, 4),
    .groups = "drop"
  )
print(shapiro_results)
cat("   → All p > 0.05 indicates no significant departure from normality.\n\n")

# ── Assumption 3: Homogeneity of Variance (Bartlett's test)
cat("3. HOMOGENEITY OF VARIANCE (Bartlett's test)\n")
bartlett_result <- bartlett.test(poc_pct ~ city, data = df)
cat("   Bartlett's K² =", round(bartlett_result$statistic, 4),
    ", df =", bartlett_result$parameter,
    ", p =", round(bartlett_result$p.value, 4), "\n")
if (bartlett_result$p.value > 0.05) {
  cat("   → p > 0.05: Equal variances assumption met. ✓\n\n")
} else {
  cat("   → p < 0.05: Unequal variances detected. Consider Welch's ANOVA.\n\n")
}

# Levene's-style check via SD ratio
sd_ratio <- max(summary_stats$sd_poc) / min(summary_stats$sd_poc)
cat("   SD ratio (max/min):", round(sd_ratio, 2),
    "(ratio < 2 generally acceptable)\n\n")

# ── 3. One-Way ANOVA ──────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════\n")
cat("  ONE-WAY ANOVA\n")
cat("═══════════════════════════════════════════════════\n\n")
cat("H₀: Mean POC representation is equal across all four fashion capitals.\n")
cat("H₁: At least one city differs significantly in mean POC representation.\n")
cat("α  = 0.05\n\n")

anova_model  <- aov(poc_pct ~ city, data = df)
anova_summary <- summary(anova_model)
print(anova_summary)

# Extract key values
f_stat <- anova_summary[[1]]$`F value`[1]
p_val  <- anova_summary[[1]]$`Pr(>F)`[1]
df_bet <- anova_summary[[1]]$Df[1]
df_wit <- anova_summary[[1]]$Df[2]

cat("\nF(", df_bet, ",", df_wit, ") =", round(f_stat, 3),
    ",  p =", formatC(p_val, format="e", digits=3), "\n")

if (p_val < 0.05) {
  cat("\n→ REJECT H₀. There is a statistically significant difference in mean\n")
  cat("  POC representation across fashion capitals (p < 0.05).\n\n")
} else {
  cat("\n→ FAIL TO REJECT H₀.\n\n")
}

# Effect size: Eta-squared (η²)
ss_between <- anova_summary[[1]]$`Sum Sq`[1]
ss_total   <- sum(anova_summary[[1]]$`Sum Sq`)
eta_sq     <- ss_between / ss_total
cat("Effect size — η² =", round(eta_sq, 4),
    "(", ifelse(eta_sq > 0.14, "large", ifelse(eta_sq > 0.06, "medium", "small")),
    "effect)\n\n")

# ── 4. Post-Hoc: Pairwise t-tests with Bonferroni Correction ─────────────────

cat("═══════════════════════════════════════════════════\n")
cat("  POST-HOC TESTS (Bonferroni correction)\n")
cat("═══════════════════════════════════════════════════\n\n")
cat("With 4 groups, there are C(4,2) = 6 pairwise comparisons.\n")
cat("Bonferroni-adjusted α = 0.05 / 6 =", round(0.05/6, 4), "\n\n")

posthoc <- pairwise.t.test(df$poc_pct, df$city,
                           p.adjust.method = "bonferroni",
                           pool.sd = TRUE)
print(posthoc)

cat("\n── Mean differences between cities ──\n")
city_means <- tapply(df$poc_pct, df$city, mean)
cities     <- names(city_means)
for (i in 1:(length(cities)-1)) {
  for (j in (i+1):length(cities)) {
    diff <- city_means[cities[i]] - city_means[cities[j]]
    cat(sprintf("  %s vs %s: Δ = %.2f pp\n",
                cities[i], cities[j], diff))
  }
}

# ── 5. Visualizations ─────────────────────────────────────────────────────────

cat("\n═══════════════════════════════════════════════════\n")
cat("  GENERATING VISUALIZATIONS\n")
cat("═══════════════════════════════════════════════════\n\n")

# ── Plot theme: clean, editorial ──
theme_fashion <- function() {
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 11, color = "#555555",
                                    hjust = 0, margin = margin(b = 15)),
    plot.caption     = element_text(size = 8, color = "#888888", hjust = 0),
    axis.title       = element_text(size = 11, face = "bold"),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eeeeee"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(20, 25, 15, 20)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 1: Line chart — POC % over time by city (the headline visual)
# ─────────────────────────────────────────────────────────────────────────────

p1 <- ggplot(df, aes(x = year, y = poc_pct, color = city, group = city)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 3, alpha = 0.9) +
  # Annotate final values
  geom_text(
    data = df %>% filter(year == 2023),
    aes(label = paste0(city, "\n", poc_pct, "%")),
    hjust = -0.1, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = city_colors_solid) +
  scale_x_continuous(
    breaks = 2015:2023,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(12, 55)
  ) +
  labs(
    title    = "POC Model Representation at the Big Four Fashion Weeks",
    subtitle = "Percentage of model appearances by non-white models, 2015–2023",
    x        = "Year",
    y        = "POC Representation (%)",
    caption  = "Source: The Fashion Spot Annual Runway Diversity Reports"
  ) +
  theme_fashion() +
  theme(legend.position = "none")

ggsave("plot1_trends.png", p1, width = 10, height = 6, dpi = 180)
cat("Plot 1 saved: Trend lines over time\n")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 2: Boxplots by city (ANOVA visualization — distributions)
# ─────────────────────────────────────────────────────────────────────────────

# Add mean labels
city_label_df <- summary_stats %>%
  mutate(label = paste0("μ = ", mean_poc, "%\nσ = ", sd_poc))

p2 <- ggplot(df, aes(x = city, y = poc_pct, fill = city)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white", outlier.size = 2.5, width = 0.5) +
  geom_jitter(aes(color = city), width = 0.08, size = 2.5,
              alpha = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 4, fill = "white", color = "black") +
  geom_text(data = city_label_df,
            aes(x = city, y = 52, label = label),
            size = 3.2, color = "#333333", inherit.aes = FALSE) +
  scale_fill_manual(values  = city_colors_solid) +
  scale_color_manual(values = city_colors_solid) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(12, 56)
  ) +
  labs(
    title    = "Distribution of POC Representation by Fashion Capital",
    subtitle = "Boxplots with individual seasons shown · Diamond = group mean · 2015–2023",
    x        = NULL,
    y        = "POC Representation (%)",
    caption  = "Source: The Fashion Spot Annual Runway Diversity Reports"
  ) +
  theme_fashion() +
  theme(legend.position = "none")

ggsave("~/Desktop/stat 311/plot2_boxplots.png", p2, width = 9, height = 6, dpi = 180)
cat("Plot 2 saved: Boxplots by city\n")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 3: Assumption check — Histograms (normality)
# ─────────────────────────────────────────────────────────────────────────────

p3 <- ggplot(df, aes(x = poc_pct, fill = city)) +
  geom_histogram(bins = 5, color = "white", alpha = 0.85) +
  facet_wrap(~ city, ncol = 2) +
  scale_fill_manual(values = city_colors_solid) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Assumption Check: Approximate Normality",
    subtitle = "Distribution of POC % per city (n = 9 per group)",
    x        = "POC Representation (%)",
    y        = "Count",
    caption  = "Small samples — normality assessed visually and via Shapiro-Wilk"
  ) +
  theme_fashion() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11))

ggsave("~/Desktop/stat 311/plot3_normality.png", p3, width = 9, height = 6, dpi = 180)
cat("Plot 3 saved: Normality histograms\n")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 4: Assumption check — SDs and variance equality
# ─────────────────────────────────────────────────────────────────────────────

p4 <- ggplot(summary_stats, aes(x = city, y = sd_poc, fill = city)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_text(aes(label = paste0("σ = ", sd_poc, "%")),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = city_colors_solid) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(summary_stats$sd_poc) * 1.3)
  ) +
  labs(
    title    = "Assumption Check: Homogeneity of Variance",
    subtitle = paste0("Standard deviations by city · SD ratio (max/min) = ",
                      round(sd_ratio, 2)),
    x        = NULL,
    y        = "Standard Deviation of POC %",
    caption  = "Bartlett's test and SD ratio both used to assess variance equality"
  ) +
  theme_fashion() +
  theme(legend.position = "none")

ggsave("~/Desktop/stat 311/plot4_variance.png", p4, width = 9, height = 5, dpi = 180)
cat("Plot 4 saved: Variance check\n")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 5: Pairwise comparison heatmap (post-hoc results)
# ─────────────────────────────────────────────────────────────────────────────

# Build a tidy matrix of Bonferroni p-values
p_mat   <- posthoc$p.value
cities4 <- c("New York", "London", "Paris", "Milan")

# Create full symmetric matrix
full_mat <- matrix(NA, 4, 4, dimnames = list(cities4, cities4))
for (i in 1:nrow(p_mat)) {
  for (j in 1:ncol(p_mat)) {
    rn <- rownames(p_mat)[i]
    cn <- colnames(p_mat)[j]
    if (rn %in% cities4 && cn %in% cities4) {
      full_mat[rn, cn] <- p_mat[i, j]
      full_mat[cn, rn] <- p_mat[i, j]
    }
  }
}
diag(full_mat) <- 1  # self-comparison

# Tidy format
heatmap_df <- as.data.frame(as.table(full_mat)) %>%
  rename(city1 = Var1, city2 = Var2, p_bonf = Freq) %>%
  mutate(
    p_bonf    = as.numeric(p_bonf),
    label     = case_when(
      city1 == city2       ~ "—",
      is.na(p_bonf)        ~ "NA",
      p_bonf < 0.001       ~ "p < 0.001***",
      p_bonf < 0.01        ~ paste0(round(p_bonf, 3), "**"),
      p_bonf < 0.05        ~ paste0(round(p_bonf, 3), "*"),
      TRUE                 ~ paste0(round(p_bonf, 3), " ns")
    ),
    significant = !is.na(p_bonf) & p_bonf < 0.05 & city1 != city2
  )

p5 <- ggplot(heatmap_df, aes(x = city1, y = city2, fill = p_bonf)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = label), size = 3.5, fontface = "bold",
            color = ifelse(heatmap_df$significant, "white", "#555555")) +
  scale_fill_gradientn(
    colors   = c("#1a1a2e", "#6b4c9a", "#ecdbba"),
    na.value = "#f0f0f0",
    limits   = c(0, 1),
    name     = "Bonferroni p-value"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Post-Hoc Pairwise Comparisons (Bonferroni Correction)",
    subtitle = "Adjusted p-values for all 6 city pairs · *** p<.001  ** p<.01  * p<.05",
    x = NULL, y = NULL,
    caption  = "Dark cells = statistically significant differences"
  ) +
  theme_fashion() +
  theme(
    axis.text.x     = element_text(face = "bold"),
    axis.text.y     = element_text(face = "bold"),
    legend.position = "right",
    panel.grid      = element_blank()
  )

ggsave("~/Desktop/stat 311/plot5_posthoc.png", p5, width = 8, height = 6, dpi = 180)
cat("Plot 5 saved: Post-hoc heatmap\n")

# ─────────────────────────────────────────────────────────────────────────────
# PLOT 6: Change from 2015 baseline — who improved most?
# ─────────────────────────────────────────────────────────────────────────────

p6 <- ggplot(df %>% filter(year > 2015),
             aes(x = year, y = change_from_2015,
                 color = city, group = city)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#aaaaaa") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    data = df %>% filter(year == 2023),
    aes(label = paste0(city, "\n+", change_from_2015, " pp")),
    hjust = -0.1, size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(values = city_colors_solid) +
  scale_x_continuous(
    breaks = 2016:2023,
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  scale_y_continuous(labels = function(x) paste0("+", x, " pp")) +
  labs(
    title    = "Diversity Progress Since 2015 by Fashion Capital",
    subtitle = "Change in POC representation (percentage points) relative to 2015 baseline",
    x        = "Year",
    y        = "Change from 2015 baseline (pp)",
    caption  = "pp = percentage points · Source: The Fashion Spot"
  ) +
  theme_fashion() +
  theme(legend.position = "none")

ggsave("~/Desktop/stat 311/plot6_progress.png", p6, width = 10, height = 6, dpi = 180)
cat("Plot 6 saved: Progress from baseline\n")

# ── 6. Final Summary ──────────────────────────────────────────────────────────

cat("\n═══════════════════════════════════════════════════\n")
cat("  ANALYSIS COMPLETE — SUMMARY\n")
cat("═══════════════════════════════════════════════════\n\n")

cat("ANOVA result:  F(", df_bet, ",", df_wit, ") =", round(f_stat, 2),
    ", p =", formatC(p_val, format = "e", digits = 2), "\n")
cat("Effect size:   η² =", round(eta_sq, 3), "(large)\n")
cat("Decision:      Reject H₀ — cities differ significantly\n\n")

cat("City rankings by mean POC representation:\n")
ranked <- sort(city_means, decreasing = TRUE)
for (i in seq_along(ranked)) {
  cat(sprintf("  %d. %s: %.1f%%\n", i, names(ranked)[i], ranked[i]))
}

cat("\nPlots saved:\n")
plots <- c("plot1_trends.png", "plot2_boxplots.png", "plot3_normality.png",
           "plot4_variance.png", "plot5_posthoc.png", "plot6_progress.png")
for (p in plots) cat("  →", p, "\n")



