#----------------------------------------------------------------
# Table SI-3 Panel B: Bond descriptive statistics by county type
#
# This script compares bond-level descriptive statistics between rural and
# urban counties. Variables are winsorized at 1st/99th percentiles. Outputs
# summary statistics (mean, SD, median, obs) as a LaTeX table.
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
library(DescTools)

# Custom formatting functions
cust_format1 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 2)}
cust_format2 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 0)}

# Winsorize function
winsorize <- function(x, p = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, probs = p, na.rm = TRUE)
  DescTools::Winsorize(x, val = q)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

spread <- read_dta("../../../data/2024/bats_county_issuers_data.dta")

#----------------------------------------------------------------
# Create summary statistics for rural and urban counties
#----------------------------------------------------------------

# Summary statistics function
multi.summary.short <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    p = quantile(x, probs = 0.5, na.rm = TRUE), Obs. = sum(!is.na(x)))
}

# Variables to winsorize
winsorize_vars <- c('spread', 'total_maturity_offering_amt_f', 'size', 'lnmat', 'lnsize',
                    'cum_outbreak_count',
                    'q1mintemp', 'q2mintemp', 'q3mintemp', 'q4mintemp',
                    'q1precip', 'q2precip', 'q3precip', 'q4precip')

# Variables for summary statistics
sel <- c('wns', 'statetaxSPREAD_MMAinterp',
         'total_maturity_offering_amt_f', 'lnsize',
         'matround', 'lnmat',
         'BQ', 'GO', 'callable', 'insured_mergent',
         'agg_rating_min_lt')

# Urban counties
urban_data <- spread %>% filter(rural_county == 0)
urban_data <- urban_data %>%
  mutate_at(vars(contains(winsorize_vars)), .funs = list(~winsorize(.)))

table_descriptives_urban <- sapply(urban_data[, sel, with = FALSE], multi.summary.short)
table_descriptives_urban <- data.frame(t(table_descriptives_urban))

# Rural counties
rural_data <- spread %>% filter(rural_county == 1)
rural_data <- rural_data %>%
  mutate_at(vars(contains(winsorize_vars)), .funs = list(~winsorize(.)))

table_descriptives_rural <- sapply(rural_data[, sel, with = FALSE], multi.summary.short)
table_descriptives_rural <- data.frame(t(table_descriptives_rural))

# Combine rural and urban tables
table_descriptives <- cbind(table_descriptives_rural, table_descriptives_urban)
names(table_descriptives) <- c('Mean_rural', 'StDev_rural', 'Median_rural', 'Obs_rural',
                               'Mean_urban', 'StDev_urban', 'Median_urban', 'Obs_urban')

# Apply formatting
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs_rural', -'Obs_urban'), cust_format1) %>%
  mutate_at(vars('Obs_rural', 'Obs_urban'), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean', 'StDev', 'Median', 'Obs.', 'Mean', 'StDev', 'Median', 'Obs.')
rownames(table_descriptives) <- c('Post',
                                  'Spread, bps',
                                  'Bond Size, USD mln', 'Log(Bond Size)',
                                  'Time to Maturity, years', 'Log(Maturity)',
                                  'BQ', 'GO', 'Callable',
                                  'Insured', 'Rating')

#----------------------------------------------------------------
# Output table to LaTeX
#----------------------------------------------------------------

# Custom header for rural/urban columns
custom_header <- list()
custom_header$pos <- list(-1)
custom_header$command <- c("  & \\multicolumn{4}{c}{Rural Counties} & \\multicolumn{4}{c}{Urban Counties} \\  \\\\\n")

print(xtable(table_descriptives, align = "lcccccccc"),
      type = 'latex',
      floating = FALSE,
      sanitize.text.function = function(x) {x},
      hline.after = c(-1, 0, nrow(table_descriptives)),
      booktabs = TRUE,
      add.to.row = custom_header,
      file = '../../tables_figures_SI/table_SI-3_panel_b.tex')
