#----------------------------------------------------------------
# Figure SI-3: Randomization inference test
#
# This script performs a randomization inference test to assess the statistical
# significance of WNS effects on municipal bond spreads. It runs 10,000 iterations,
# randomly assigning placebo WNS treatment to counties and estimating the effect
# on bond spreads. The output is a density plot comparing the distribution of
# placebo estimates to the actual estimated effect. 
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(fixest)
library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(haven)

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

spread <- read_dta("../../../data/2024/bats_rural_spread_data.dta")

# WNS data
wns <- read_excel("../../../raw_data/unified WNS file/unified_wns_dates.xlsx")

#----------------------------------------------------------------
# Run randomization inference test (10,000 iterations)
#----------------------------------------------------------------

coefficients = data.table()

for (i in 1:10000) {

  set.seed(i)
  print(i)

  # Randomly assign WNS treatment status and treatment years
  wns_random <- wns %>% dplyr::select(STATEFP, COUNTYFP, fips)
  wns_random$wns_random <- sample(c(0, 1), size = nrow(wns_random), replace = TRUE)
  wns_random_treatment <- wns_random %>% filter(wns_random == 1)
  wns_random_treatment$treatment_year_random <- sample(2001:2022, size = nrow(wns_random_treatment), replace = TRUE)
  wns_random <- left_join(wns_random, wns_random_treatment)
  wns_random$treatment_year_random <- ifelse(is.na(wns_random$treatment_year_random), 0, wns_random$treatment_year_random)

  # Merge randomized treatment with spread data
  spread_short <- spread %>%
    dplyr::select(statetaxSPREAD_MMAinterp, lnsize, lnmat, BQ, GO, callable,
                  insured_mergent, log_pop, cum_outbreak_count,
                  q1mintemp, q2mintemp, q3mintemp, q4mintemp,
                  q1precip, q2precip, q3precip, q4precip,
                  identif, year, agg_rating_min_lt, fips)
  spread_short <- left_join(spread_short, wns_random)

  # Create placebo treatment indicator
  spread_short$wns <- ifelse(spread_short$year > spread_short$treatment_year_random &
                               spread_short$wns_random == 1, 1, 0)
  spread_short$wns <- ifelse(is.na(spread_short$wns), 0, spread_short$wns)

  # Run regression with placebo treatment
  reg <- feols(statetaxSPREAD_MMAinterp ~ wns + lnsize + lnmat + BQ + GO + callable +
                 insured_mergent + log_pop + cum_outbreak_count +
                 q1mintemp + q2mintemp + q3mintemp + q4mintemp +
                 q1precip + q2precip + q3precip + q4precip
               | identif + year + agg_rating_min_lt,
               cluster ~ identif + year,
               fixef.rm = "singleton",
               data = spread_short)

  # Store estimate
  estimate <- data.table(reg$coeftable)[1, ]
  estimate$n_observations <- reg$nobs
  coefficients <- rbind(coefficients, estimate)
}

#----------------------------------------------------------------
# Create density plot
#----------------------------------------------------------------

# Calculate 95% confidence interval bounds
conf_min <- 11.473 - 1.96 * 3.525
conf_max <- 11.473 + 1.96 * 3.525

# Share of placebo estimates falling within actual CI
sum(coefficients$Estimate >= conf_min & coefficients$Estimate <= conf_max) / nrow(coefficients)

p <- ggplot(coefficients, aes(x = Estimate)) +
  # 95% CI band (grey)
  annotate("rect",
           xmin = conf_min, xmax = conf_max,
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.25) +
  # Density curve
  geom_density(fill = "#d01c8b", alpha = 0.20, colour = "#d01c8b", linewidth = 3) +
  # Vertical line at actual estimate
  geom_vline(xintercept = 11.47, linetype = "dashed", colour = "black", linewidth = 2) +
  scale_x_continuous(
    limits = c(-19, 19),
    breaks = c(-10, -5, 0, 5, 10, 11.47),
    labels = c("-10", "-5", "0", "5", "10", "11.47")
  ) +
  theme_minimal() +
  theme(
    text             = element_text(family = "Helvetica"),
    axis.text.x      = element_text(size = 40, colour = "black"),
    axis.text.y      = element_text(size = 40, colour = "black"),
    axis.ticks.x     = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 45, colour = "black")
  ) +
  labs(x = "Estimate", y = "Density")

p

ggsave('../../tables_figures_SI/figure_SI-3.pdf', p,
       width = 35, height = 10, dpi = 300, units = "in")
