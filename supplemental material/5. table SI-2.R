#----------------------------------------------------------------
# Table SI-2: Rural county descriptive statistics
#
# This script generates descriptive statistics for rural county-level data.
# Panel A: Summary statistics (mean, SD, percentiles) for fiscal and control variables.
# Panel B: Pairwise correlation matrix.
# Outputs are saved as LaTeX tables.
#----------------------------------------------------------------

rm(list = ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(dplyr)
library(data.table)
library(haven)
library(xtable)

# Custom formatting functions
cust_format1 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 2)}
cust_format2 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 0)}

# Summary statistics function
multi.summary <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    p = quantile(x, probs = c(.1, .25, .5, .75, .9), na.rm = TRUE),
    Obs. = sum(!is.na(x)))
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

rural_data <- read_dta("../../../data/2024/bats_rural_financial_data.dta")

#----------------------------------------------------------------
# Panel A: Summary statistics
#----------------------------------------------------------------

sel <- c('wns',
         'property_tax_per_capita',
         'total_rev_own_sources_per_capita',
         'total_tax_per_capita',
         'total_revenue_per_capita',
         'population', 'lnpop',
         'cum_outbreak_count',
         'q1mintemp', 'q2mintemp', 'q3mintemp', 'q4mintemp',
         'q1precip', 'q2precip', 'q3precip', 'q4precip')

# Create and format table
table_descriptives <- sapply(rural_data[, sel, with = FALSE], multi.summary)
table_descriptives <- t(table_descriptives)
table_descriptives <- data.frame(table_descriptives)

# Apply formatting
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs.'), cust_format1) %>%
  mutate_at(vars('Obs.'), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean', 'StDev', 'p$^{10\\%}$', 'p$^{25\\%}$', 'p$^{50\\%}$',
                                  'p$^{75\\%}$', 'p$^{90\\%}$', 'Obs.')

rownames(table_descriptives) <- c('Post',
                                  'Property tax p.c.', 'Total revenue own p.c.', 'Total tax p.c.', 'Revenue p.c.',
                                  'Population', 'Log(Population)', 'Neighbors with WNS',
                                  'Q1 avg. min. temp.', 'Q2 avg. min. temp.', 'Q3 avg. min. temp.', 'Q4 avg. min. temp.',
                                  'Q1 avg. precip.', 'Q2 avg. precip.', 'Q3 avg. precip.', 'Q4 avg. precip.')

# Output Panel A
print(xtable(table_descriptives, align = "lcccccccc"),
      type = 'latex',
      floating = FALSE,
      sanitize.text.function = function(x) {x},
      hline.after = c(-1, 0, nrow(table_descriptives)),
      booktabs = TRUE,
      file = '../../tables_figures_SI/table_SI-2_panel_a.tex')

#----------------------------------------------------------------
# Panel B: Pairwise correlation table
#----------------------------------------------------------------

# Pearson correlation (lower triangle)
corr_table1 <- cor(rural_data[, sel, with = FALSE], use = "complete.obs")
corr_table1[upper.tri(corr_table1, diag = TRUE)] <- 0

# Spearman correlation (upper triangle, not used in final output)
corr_table2 <- cor(rural_data[, sel, with = FALSE], use = "complete.obs", method = 'spearman')
corr_table2[lower.tri(corr_table2, diag = FALSE)] <- 0

# Use Pearson correlations only
corr_table <- corr_table1

# Format correlation table
corr_table[corr_table == 1] <- NA
corr_table[corr_table == 0] <- NA
corr_table <- round(corr_table, 2)

rownames(corr_table) <- paste0('{[', seq_along(sel), ']} ',
                               c('Post', 'Property tax p.c.', 'Total revenue own p.c.', 'Total tax p.c.', 'Revenue p.c.',
                                 'Population', 'Log(Population)', 'Neighbors with WNS',
                                 'Q1 avg. min. temp.', 'Q2 avg. min. temp.', 'Q3 avg. min. temp.', 'Q4 avg. min. temp.',
                                 'Q1 avg. precip.', 'Q2 avg. precip.', 'Q3 avg. precip.', 'Q4 avg. precip.'))
colnames(corr_table) <- paste0('[', seq_along(sel), ']')

# Output Panel B
print(xtable(corr_table, align = c('l', rep('c', ncol(corr_table))), digits = 2),
      type = 'latex',
      floating = FALSE,
      sanitize.text.function = function(x) {x},
      hline.after = c(-1, 0, nrow(corr_table)),
      booktabs = TRUE,
      file = '../../tables_figures_SI/table_SI-2_panel_b.tex')
