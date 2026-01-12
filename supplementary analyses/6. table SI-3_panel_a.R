#----------------------------------------------------------------
# Table SI-3 Panel A: Agricultural property tax share comparison
#
# This script compares the share of property taxes collected from agricultural
# properties between rural and urban counties. It merges CoreLogic tax data
# with county classification data and outputs summary statistics as a LaTeX table.
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
library(readxl)
library(arrow)

# Custom formatting functions
cust_format1 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 2)}
cust_format2 <- function(x, digits) {formatC(x, format = "f", big.mark = ",", digits = 0)}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

# Tax data by county-year and county classification data
tax.data <- read_parquet('../../../raw_data/CoreLogic data/agtax/AA_Eli_stats_output_42480450/results/AA_Eli_stats_county_year_tax.parquet')
cnty.data <- read_excel('../../../raw_data/Urban county data/rural_counties_definition.xlsx')

# Pad FIPS codes with leading zeros if needed
cnty.data <- cnty.data %>%
  mutate(fips_code = ifelse(nchar(as.character(fips)) == 4,
                            paste0('0', as.character(fips)),
                            as.character(fips)))

# Merge tax data with county classification
cnty.data <- tax.data %>%
  left_join(cnty.data, by = 'fips_code')
cnty.data$year <- as.numeric(cnty.data$year)

# Load main county-year level data
data <- read_dta("../../../data/2024/bats_county_financial_data.dta")
data <- left_join(data, cnty.data)

#----------------------------------------------------------------
# Create summary statistics
#----------------------------------------------------------------

rural_data <- data %>% filter(rural_county == 1)
urban_data <- data %>% filter(rural_county == 0)

# Summary statistics function
multi.summary.short <- function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    p = quantile(x, probs = 0.5, na.rm = TRUE), Obs. = sum(!is.na(x)))
}

sel <- c("ag_share_tax")

# Create and format table
table_descriptives_rural <- sapply(rural_data[, sel, with = FALSE], multi.summary.short)
table_descriptives_rural <- data.frame(t(table_descriptives_rural))

table_descriptives_urban <- sapply(urban_data[, sel, with = FALSE], multi.summary.short)
table_descriptives_urban <- data.frame(t(table_descriptives_urban))

table_descriptives <- cbind(table_descriptives_rural, table_descriptives_urban)
names(table_descriptives) <- c('Mean_rural', 'StDev_rural', 'Median_rural', 'Obs_rural',
                               'Mean_urban', 'StDev_urban', 'Median_urban', 'Obs_urban')

# Apply formatting
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs_rural', -'Obs_urban'), cust_format1) %>%
  mutate_at(vars('Obs_rural', 'Obs_urban'), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean', 'StDev', 'Median', 'Obs.', 'Mean', 'StDev', 'Median', 'Obs.')
rownames(table_descriptives) <- c('\\% Property Tax Collected from Agricultural Properties')

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
      file = '../../tables_figures_SI/table_SI-3_panel_a.tex')