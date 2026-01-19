#----------------------------------------------------------------
# Figure 2, Panel A: Cross-sectional heterogeneity in property tax effects.
#
# This script runs regressions of property tax revenue per capita on WNS
# for different subsamples (all, high/low bats at risk, high/low assessment
# frequency), then creates a coefficient plot with 95%, 90%, and 85% CIs.
#----------------------------------------------------------------

rm(list = ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(fixest)
library(dplyr)
library(ggplot2)
library(haven)
library(showtext)

showtext_auto()

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta("../../../data/2024/bats_rural_financial_data_main.dta")

#----------------------------------------------------------------
# Regressions on the rural subsample
#----------------------------------------------------------------

high_bats <- feols(property_tax_per_capita ~ wns
                   + lnpop
                   + cum_outbreak_count
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip + q4precip
                   | year + fips,
                   cluster ~ fips,
                   fixef.rm = "singleton",
                   data = data %>% filter(high_bats == 1))
summary(high_bats)

low_bats <- feols(property_tax_per_capita ~ wns
                  + lnpop
                  + cum_outbreak_count
                  + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                  + q1precip + q2precip + q3precip + q4precip
                  | year + fips,
                  cluster ~ fips,
                  fixef.rm = "singleton",
                  data = data %>% filter(high_bats == 0))
summary(low_bats)

high_freq <- feols(property_tax_per_capita ~ wns
                   + lnpop
                   + cum_outbreak_count
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip + q4precip
                   | year + fips,
                   cluster ~ fips,
                   fixef.rm = "singleton",
                   data = data %>% filter(low_assesment_frequency == 0))
summary(high_freq)

low_freq <- feols(property_tax_per_capita ~ wns
                  + lnpop
                  + cum_outbreak_count
                  + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                  + q1precip + q2precip + q3precip + q4precip
                  | year + fips,
                  cluster ~ fips,
                  fixef.rm = "singleton",
                  data = data %>% filter(low_assesment_frequency == 1))
summary(low_freq)

all <- feols(property_tax_per_capita ~ wns
             + lnpop
             + cum_outbreak_count
             + q1mintemp + q2mintemp + q3mintemp + q4mintemp
             + q1precip + q2precip + q3precip + q4precip
             | year + fips,
             cluster ~ fips,
             fixef.rm = "singleton",
             data = data)
summary(all)

#----------------------------------------------------------------
# Visualize the coefficients
#----------------------------------------------------------------

# Combine into a dataframe (95%)
results95 <- data.frame(
  Name = rbind("All",
               "High Bats\nat Risk",
               "Low Bats\nat Risk",
               "High Assessment\nFreq.",
               "Low Assessment\nFreq."),
  Estimate = rbind(coef(all)["wns"], coef(high_bats)["wns"], coef(low_bats)["wns"], coef(high_freq)["wns"], coef(low_freq)["wns"]),
  CI_low = rbind(confint(all, parm = "wns", level = 0.95)[1], confint(high_bats, parm = "wns", level = 0.95)[1], confint(low_bats, parm = "wns", level = 0.95)[1], confint(high_freq, parm = "wns", level = 0.95)[1], confint(low_freq, parm = "wns", level = 0.95)[1]),
  CI_high = rbind(confint(all, parm = "wns", level = 0.95)[2], confint(high_bats, parm = "wns", level = 0.95)[2], confint(low_bats, parm = "wns", level = 0.95)[2], confint(high_freq, parm = "wns", level = 0.95)[2], confint(low_freq, parm = "wns", level = 0.95)[2])
)

names(results95) <- c("Name", "Estimate", "CI_low", "CI_high")
results95$CI <- "95% CI"
results95$x <- c(-16, -14, -12, -10, -8)
results95$Name <- factor(results95$Name, levels = c(
  "All",
  "High Bats\nat Risk",
  "Low Bats\nat Risk",
  "High Assessment\nFreq.",
  "Low Assessment\nFreq."
))

# Combine into a dataframe (90%)
results90 <- data.frame(
  Name = rbind("All",
               "High Bats\nat Risk",
               "Low Bats\nat Risk",
               "High Assessment\nFreq.",
               "Low Assessment\nFreq."),
  Estimate = rbind(coef(all)["wns"], coef(high_bats)["wns"], coef(low_bats)["wns"], coef(high_freq)["wns"], coef(low_freq)["wns"]),
  CI_low = rbind(confint(all, parm = "wns", level = 0.90)[1], confint(high_bats, parm = "wns", level = 0.90)[1],
                 confint(low_bats, parm = "wns", level = 0.90)[1], confint(high_freq, parm = "wns", level = 0.90)[1],
                 confint(low_freq, parm = "wns", level = 0.90)[1]),
  CI_high = rbind(confint(all, parm = "wns", level = 0.90)[2], confint(high_bats, parm = "wns", level = 0.90)[2],
                  confint(low_bats, parm = "wns", level = 0.90)[2],
                  confint(high_freq, parm = "wns", level = 0.90)[2], confint(low_freq, parm = "wns", level = 0.90)[2])
)

names(results90) <- c("Name", "Estimate", "CI_low", "CI_high")
results90$CI <- "90% CI"
results90$x <- c(-16, -14, -12, -10, -8)
results90$Name <- factor(results90$Name, levels = c(
  "All",
  "High Bats\nat Risk",
  "Low Bats\nat Risk",
  "High Assessment\nFreq.",
  "Low Assessment\nFreq."
))

# Combine into a dataframe (85%)
results85 <- data.frame(
  Name = rbind("All",
               "High Bats\nat Risk",
               "Low Bats\nat Risk",
               "High Assessment\nFreq.",
               "Low Assessment\nFreq."),
  Estimate = rbind(coef(all)["wns"], coef(high_bats)["wns"], coef(low_bats)["wns"], coef(high_freq)["wns"], coef(low_freq)["wns"]),
  CI_low = rbind(confint(all, parm = "wns", level = 0.85)[1], confint(high_bats, parm = "wns", level = 0.85)[1],
                 confint(low_bats, parm = "wns", level = 0.85)[1], confint(high_freq, parm = "wns", level = 0.85)[1],
                 confint(low_freq, parm = "wns", level = 0.85)[1]),
  CI_high = rbind(confint(all, parm = "wns", level = 0.85)[2], confint(high_bats, parm = "wns", level = 0.85)[2],
                  confint(low_bats, parm = "wns", level = 0.85)[2],
                  confint(high_freq, parm = "wns", level = 0.85)[2], confint(low_freq, parm = "wns", level = 0.85)[2])
)

names(results85) <- c("Name", "Estimate", "CI_low", "CI_high")
results85$CI <- "85% CI"
results85$x <- c(-16, -14, -12, -10, -8)
results85$Name <- factor(results85$Name, levels = c(
  "All",
  "High Bats\nat Risk",
  "Low Bats\nat Risk",
  "High Assessment\nFreq.",
  "Low Assessment\nFreq."
))

results <- rbind(results95, results90)
results <- rbind(results, results85)

# Create the plot
bar <- ggplot(results, aes(x = Name, y = Estimate)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = CI),
                width = 0.1,
                linewidth = 1) +
  scale_color_manual(
    values = c("95% CI" = "grey80",
               "90% CI" = "grey55",
               "85% CI" = "black"),
    breaks = c("95% CI", "90% CI", "85% CI")
  ) +
  geom_point(aes(y = Estimate),
             shape = 21, size = 5, stroke = 1.5, fill = "black", color = "black") +
  geom_text(aes(label = paste0(Name, " (", round(Estimate, 2), ")")),
            y = max(results$CI_high) + 0.6,
            vjust = 0,
            size = 14,
            family = "Helvetica") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 2) +
  geom_hline(yintercept = -75.63, linetype = "dashed", color = "black", size = 2) +
  labs(
    y = "Property tax revenue per capita",
    x = NULL
  ) +
  scale_y_continuous(
    limits = c(-450, 450),
    breaks = c(-200, -75.63, 0, 50, 100, 200),
    labels = c("-200", "-75.63", "0", "50", "100", "200")
  ) +
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 1.2))) +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 50, color = "black"),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 55, family = "Helvetica", color = "black"),
    legend.text = element_text(size = 45),
    legend.position = "bottom"
  )

bar

ggsave(paste0('../../tables_figures_main/', 'figure2_panel_a.pdf'), bar,
       width = 32, height = 10, dpi = 300, units = "in")
