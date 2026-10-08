# ==============================================================================
# E-Commerce Order Revenue: Explanatory and Predictive Modelling
# SweetAroma - analysis of July 2025 order data
# ==============================================================================

# --- Setup --------------------------------------------------------------
# Run once if packages aren't installed:
# install.packages(c("corrgram", "ggplot2", "misty", "VIM", "mice",
#                     "patchwork", "lmtest", "cowplot", "dplyr"))

library(cowplot)
library(lmtest)
library(patchwork)
library(mice)
library(VIM)
library(misty)
library(corrgram)
library(dplyr)
library(ggplot2)

# --- Load data ------------------------------------------------------------

july25 <- read.csv("data/order_july25.csv", header = TRUE)
head(july25)

# ==============================================================================
# Summary statistics
# ==============================================================================

summary(july25)
sd(july25$revenue, na.rm = TRUE)
sd(july25$time_web, na.rm = TRUE)
sd(july25$past_spend, na.rm = TRUE)

#' Percentage frequency table for a categorical variable, excluding NAs
freq_table <- function(x) {
  data.frame(100 * table(x[!is.na(x)]) / length(x[!is.na(x)]))
}

voucher_freq <- freq_table(july25$voucher)
voucher_freq
ad_channel_freq <- freq_table(july25$ad_channel)
ad_channel_freq
past_order_freq <- freq_table(july25$number_past_order)
past_order_freq

# --- Frequency visualisations -----------------------------------------------

#' Histogram of a continuous variable
plot_histogram <- function(data, var, xlab) {
  ggplot(data) +
    geom_histogram(aes(x = .data[[var]]), bins = 30) +
    labs(x = xlab, y = "Count")
}

#' Bar chart of a discrete/categorical variable
plot_bar <- function(data, var, xlab) {
  ggplot(data) +
    geom_bar(aes(x = factor(.data[[var]]))) +
    labs(x = xlab, y = "Count")
}

#' Pie chart from a pre-computed frequency table (columns: Var1, Freq)
plot_pie <- function(freq_table, legend_title) {
  ggplot(freq_table, aes(x = "", y = Freq, fill = Var1)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(round(Freq, 2), "%")),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    labs(x = NULL, y = NULL, fill = legend_title)
}

f1 <- plot_histogram(july25, "revenue", "Revenue (GBP)")
f2 <- plot_histogram(july25, "time_web", "Time on Website (seconds)")
f3 <- plot_histogram(july25, "past_spend", "Past Spend (GBP)")
f4 <- plot_bar(july25, "number_past_order", "Number of Past Orders")
f5 <- plot_pie(ad_channel_freq, "Ad Channel")
f6 <- plot_pie(voucher_freq, "Voucher")

plot_bar(july25, "ad_channel", "Ad Channel")
plot_bar(july25, "voucher", "Voucher")

# combine plots
(f1 + f2 + f3) /
  (f4 + f5 + f6)

# ==============================================================================
# Handling missing data
# ==============================================================================

sum(!complete.cases(july25)) # Rows with missing data = 581

# Testing for MCAR
na.test(july25)
# p = 0.415 > 0.05, so we cannot reject the null hypothesis that data is MCAR

# Visualise missing data
aggr(july25, numbers = TRUE, prop = FALSE, cex.axis = .9, oma = c(8, 5, 1, 2))

# Create a missing-data indicator column
missdata_july25 <- july25
missdata_july25$missing <- as.numeric(!complete.cases(july25))

# Check whether missingness correlates with any variable
# (pairwise.complete.obs needed since most columns contain NAs)
cor(missdata_july25, use = "pairwise.complete.obs")
corrgram(missdata_july25)
filter(missdata_july25, missing == 1)
# Looks random with respect to other variables - consistent with MCAR

# Listwise deletion
newjuly25 <- july25[complete.cases(july25), ]

cor(newjuly25)
corrgram(newjuly25)

# --- Comparing distributions before and after cleaning ----------------------

completedata_july25 <- july25
completedata_july25$complete <- as.numeric(complete.cases(july25))

deletion_labels <- scale_fill_discrete(
  name = "Listwise Deletion Status",
  labels = c("0" = "Before", "1" = "After")
)

p1 <- ggplot(completedata_july25) +
  geom_histogram(aes(x = time_web, fill = factor(complete))) +
  deletion_labels +
  labs(x = "Time on website (seconds)", y = "Count")

p2 <- ggplot(completedata_july25) +
  geom_histogram(aes(x = past_spend, fill = factor(complete))) +
  deletion_labels +
  labs(x = "Past Spend (GBP)", y = "Count")

p3 <- ggplot(completedata_july25[complete.cases(completedata_july25$number_past_order), ]) +
  geom_bar(aes(x = number_past_order, fill = factor(complete))) +
  deletion_labels +
  labs(x = "Number of Past Orders", y = "Count")

p4 <- ggplot(completedata_july25[complete.cases(completedata_july25$voucher), ]) +
  geom_bar(aes(x = factor(voucher), fill = factor(complete))) +
  deletion_labels +
  labs(x = "Voucher", y = "Count")

p5 <- ggplot(completedata_july25[complete.cases(completedata_july25$ad_channel), ]) +
  geom_bar(aes(x = factor(ad_channel), fill = factor(complete))) +
  deletion_labels +
  labs(x = "Advertisement Channel", y = "Count")

legend <- cowplot::get_legend(p1)
legend_panel <- ggdraw() + draw_grob(legend)

(p1 + p2 + p3) /
  (p4 + p5 + legend_panel) +
  plot_layout(guides = "collect")

# --- Comparing summary stats before and after deletion -----------------------

compare_stats <- function(before, after, label) {
  cat("\n---", label, "---\n")
  cat("Mean (before/after):", mean(before, na.rm = TRUE), "/", mean(after), "\n")
  cat("SD   (before/after):", sd(before, na.rm = TRUE), "/", sd(after), "\n")
}

compare_stats(july25$time_web, newjuly25$time_web, "Time on website")
compare_stats(july25$past_spend, newjuly25$past_spend, "Past spend")
compare_stats(july25$number_past_order, newjuly25$number_past_order, "Number of past orders")
# Means and SDs are near-identical before/after deletion, consistent with MCAR

# ==============================================================================
# Dummification
# ==============================================================================

plotnewjuly25 <- newjuly25 # copy for plotting before dummification

# One-hot encode the categorical ad_channel variable (reference level = 1)
newjuly25$ad_channel_2 <- ifelse(newjuly25$ad_channel == 2, 1, 0)
newjuly25$ad_channel_3 <- ifelse(newjuly25$ad_channel == 3, 1, 0)
newjuly25$ad_channel_4 <- ifelse(newjuly25$ad_channel == 4, 1, 0)
newjuly25 <- newjuly25 %>% select(-ad_channel)

cor(newjuly25 %>% select(number_past_order, time_web, past_spend, revenue))
corrgram(
  newjuly25 %>% select(number_past_order, time_web, past_spend, revenue),
  lower.panel = panel.shade,
  upper.panel = panel.cor,
  text.panel  = panel.txt,
  cex.labels = 1.2,
  cex = 2
)

# ==============================================================================
# Checking linear regression assumptions
# ==============================================================================

a1 <- ggplot(plotnewjuly25) + geom_point(aes(x = time_web, y = revenue)) +
  labs(x = "Time on Website (seconds)", y = "Revenue (GBP)")

a2 <- ggplot(plotnewjuly25) + geom_point(aes(x = past_spend, y = revenue)) +
  labs(x = "Past Spend (GBP)", y = "Revenue (GBP)")

a3 <- ggplot(plotnewjuly25) + geom_boxplot(aes(x = factor(number_past_order), y = revenue)) +
  labs(x = "Number of Past Orders", y = "Revenue (GBP)")

a4 <- ggplot(plotnewjuly25) + geom_boxplot(aes(x = factor(voucher), y = revenue)) +
  labs(x = "Voucher", y = "Revenue (GBP)")

a5 <- ggplot(plotnewjuly25) + geom_boxplot(aes(x = factor(ad_channel), y = revenue)) +
  labs(x = "Ad Channel", y = "Revenue (GBP)")

(a1 + a2 + a3) /
  (a4 + a5 + plot_spacer())

# ==============================================================================
# Explanatory model
# ==============================================================================

explanatory_model <- lm(revenue ~ ., data = newjuly25)
summary(explanatory_model)
summary(explanatory_model)$coefficients

# ==============================================================================
# Predictive model
# ==============================================================================

# Remove non-significant predictors iteratively
predictive_model <- lm(revenue ~ time_web + voucher + past_spend, data = newjuly25)
summary(predictive_model)
summary(predictive_model)$coefficients

final_model <- lm(revenue ~ time_web + voucher, data = newjuly25)
summary(final_model)
summary(final_model)$coefficients

# --- Predict revenue for new customers ---------------------------------------

new_customer <- read.csv("data/new_customer25.csv", header = TRUE)
prediction <- predict(final_model, newdata = new_customer)
prediction
head(new_customer)
new_customer$revenue_prediction <- prediction

# ==============================================================================
# Checking regression assumptions
# ==============================================================================

# Homoscedasticity / normality of residuals / leverage
par(mfrow = c(2, 2))
plot(explanatory_model)

par(mfrow = c(1, 4))
plot(final_model)

# Independence of errors (Durbin-Watson test)
dwtest(explanatory_model)
dwtest(final_model)

# ==============================================================================
# Model evaluation
# ==============================================================================

cat("Explanatory model - R-squared:", summary(explanatory_model)$r.squared,
    "| Adjusted R-squared:", summary(explanatory_model)$adj.r.squared, "\n")

cat("Final predictive model - R-squared:", summary(final_model)$r.squared,
    "| Adjusted R-squared:", summary(final_model)$adj.r.squared, "\n")
