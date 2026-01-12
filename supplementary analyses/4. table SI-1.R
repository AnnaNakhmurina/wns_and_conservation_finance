#----------------------------------------------------------------
# Table SI-1: Descriptive statistics
#
# This script generates descriptive statistics tables for bond-level data.
# Panel A: Summary statistics (mean, SD, percentiles) for key variables.
# Panel B: Pairwise correlation matrix.
# Panel C: Additional descriptive statistics for yield and offering variables.
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

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta("../../../data/2024/bats_rural_spread_data.dta")

data <- as.data.table(data)

#----------------------------------------------------------------
# Construct descriptive statistics
#----------------------------------------------------------------

# Summary statistics function
multi.summary <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    p = quantile(x, probs = c(.1, .25, .5, .75, .9), na.rm = TRUE),
    Obs. = sum(!is.na(x)))
}

# Panel A variables
sel <- c('wns', 'statetaxSPREAD_MMAinterp',
         'total_maturity_offering_amt_f', 'lnsize',
         'matround', 'lnmat',
         'BQ', 'GO', 'callable', 'insured_mergent',
         'agg_rating_min_lt')

# Create and format table
table_descriptives <- sapply(data[, sel, with = FALSE], multi.summary)
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
                                  'Spread, bps',
                                  'Bond Size, USD mln', 'Log(Bond Size)',
                                  'Time to Maturity, years', 'Log(Maturity)',
                                  'BQ', 'GO', 'Callable',
                                  'Insured', 'Rating')

# Create Pairwise correlation table 
sel <- c(
  'wns','statetaxSPREAD_MMAinterp',  'lnsize',
  'lnmat',
  'BQ', 'GO', 'callable', 'insured_mergent',
  "agg_rating_min_lt"
)
# Pearson Correlation Below
corr_table1 <- cor(data[, sel, with = F], use = "complete.obs")
corr_table1[upper.tri(corr_table1, diag = TRUE)] <- 0

# Spearman Correlation Above
corr_table2 <- cor(data[, sel, with = F],  use = "complete.obs", method = 'spearman')
corr_table2[lower.tri(corr_table2, diag = FALSE)] <- 0

# Create joint matrix
corr_table = corr_table1 #+ corr_table2

# Rename columns and adjust rounding
corr_table[corr_table == 1] <- NA
corr_table[corr_table == 0] <- NA
corr_table <- round(corr_table, 2)
rownames(corr_table)  <- paste0('{[', seq(1, length(sel), 1), ']} ',   c(    'Post',   'Spread, bps',
                                                                              "Log(Bond Size)",
                                                                             'Log(Maturity)', 
                                                                             'BQ', 'GO',  'Callable',
                                                                             'Insured', 'Rating'
                                                                             
)

)
colnames(corr_table) <- paste0('[', seq(1, length(sel), 1), ']')

# Output tables to LaTeX

# Aggregate descriptive table
print(xtable(table_descriptives, align = "lcccccccc"),
      type = 'latex',
      floating = F,
      sanitize.text.function = function(x){x},
      hline.after = c(-1, 0, nrow(table_descriptives)),
      booktabs = T,
      file = '../../tables_figures_SI/table_SI-1_panel_a.tex')

# Correlation table
print(xtable(corr_table, align = c('l', rep('c', ncol(corr_table))), digits = 3),
      type = 'latex',
      floating = F,
      sanitize.text.function = function(x){x},
      hline.after = c(-1, 0, nrow(corr_table)),
      booktabs = T,
      file = '../../tables_figures_SI/table_SI-1_panel_b.tex')

#----------------------------------------------------------------
# Descriptives just for example
#----------------------------------------------------------------

sel <- c( 'wns',
  'statetaxSPREAD_MMAinterp', 'offering_yield_f', "mma_linear_interp",
  'coupon_f', 
 "total_maturity_offering_amt_f", "matround",
 'total_offering_amount_f'
)


# Create and format table
table_descriptives <- sapply(data[, sel, with = F], multi.summary)
table_descriptives <- t(table_descriptives)
table_descriptives <- data.frame(table_descriptives)

# Custom number format functions
# Trailing zeros
cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
# No trailing zero
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}
# Transform
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs.'), cust_format1)
table_descriptives <- table_descriptives %>%
  mutate_at(vars('Obs.'), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean',  'StDev', 'p$^{10\\%}$', 'p$^{25\\%}$','p$^{50\\%}$',
                                  'p$^{75\\%}$', 'p$^{90\\%}$', 'Obs.')

rownames(table_descriptives) <- c('Post',
  'Spread, bps', 'Offering Yield, percent',
  "MMA Yield, percent", 'Coupon Rate, percent', 
 "Bond Size, USD mln",
  'Time to Maturity, years',
 "Offering Size, USD mln"
)


# Output tables to LaTeX
print(xtable(table_descriptives, align = "lcccccccc"),
      type = 'latex',
      floating = F,
      sanitize.text.function = function(x){x},
      hline.after = c(-1, 0, nrow(table_descriptives)),
      booktabs = T,
      file = '../../tables_figures_SI/table_SI-1_panel_c.tex')

