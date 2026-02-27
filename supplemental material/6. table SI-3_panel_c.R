
#----------------------------------------------------------------
# Table SI-3 panel C: Rural vs urban county descriptive statistics
#
# This script generates a LaTeX table comparing summary statistics
# (mean, SD, median, obs.) for rural and urban counties, including
# fiscal variables, population, WNS exposure, and weather controls.
#----------------------------------------------------------------

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(fixest)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  library(data.table)
  library(haven)
  library(xtable)
  library(DescTools)
}

#----------------------------------------------------------------
# Custom functions
#----------------------------------------------------------------

cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}

# Winsorize function
winsorize <- function(x, p = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, probs = p, na.rm = TRUE)
  DescTools::Winsorize(x, val = q)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta( "../../../data/2024/bats_county_financial_data.dta") %>% filter( !is.na(property_tax_per_capita)  & !is.na(q1precip) )

#----------------------------------------------------------------
# Define variables for summary statistics
#----------------------------------------------------------------

sel <- c(
  'wns',
  'property_tax_per_capita',
  'total_rev_own_sources_per_capita',
  'total_tax_per_capita',
  'total_revenue_per_capita',
  'population', 'lnpop',
  "cum_outbreak_count",
  "q1mintemp", "q2mintemp", "q3mintemp", "q4mintemp",
  "q1precip", "q2precip", "q3precip", "q4precip"
)

# Variables to winsorize
vars_to_winsorize <- c('property_tax_per_capita', "total_rev_own_sources_per_capita", "lnpop",
                       "cum_outbreak_count",
                       "q1mintemp", "q2mintemp", "q3mintemp", "q4mintemp",
                       "q1precip", "q2precip", "q3precip", "q4precip")

#----------------------------------------------------------------
# Rural counties
#----------------------------------------------------------------

rural_data <- data %>% filter(rural_county == 1)
# Winsorize continuous variables
rural_data <- rural_data %>%
  mutate_at(vars(contains(vars_to_winsorize)), .funs = list(~winsorize(.)))

#----------------------------------------------------------------
# Urban counties
#----------------------------------------------------------------

urban_data <- data %>% filter(rural_county == 0)
urban_data <- urban_data %>%
  mutate_at(vars(contains(vars_to_winsorize)), .funs = list(~winsorize(.)))

#----------------------------------------------------------------
# Generate summary statistics
#----------------------------------------------------------------

multi.summary.short <- function(x) {
  c(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T),
    p = quantile(x, probs = c( .5 ), na.rm = T), Obs. = sum(!is.na(x)))}

# Create and format table
table_descriptives_urban <- sapply(urban_data[, sel, with = F], multi.summary.short)
table_descriptives_urban <- t(table_descriptives_urban)
table_descriptives_urban <- data.frame(table_descriptives_urban)

table_descriptives_all <- sapply(rural_data[, sel, with = F], multi.summary.short)
table_descriptives_all <- t(table_descriptives_all)
table_descriptives_all <- data.frame(table_descriptives_all)

table_descriptives = cbind(  table_descriptives_all, table_descriptives_urban)
names(table_descriptives) <- c('Mean_urban',  'StDev_urban', 'Median_urban',  'Obs_urban', 'Mean_all',  'StDev_all', 'Median_all',  'Obs_all')

# Format numeric columns
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs_urban', - "Obs_all"), cust_format1)
table_descriptives <- table_descriptives %>%
  mutate_at(vars('Obs_urban',"Obs_all"), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean',  'StDev', 'Median',  'Obs.', 'Mean',  'StDev', 'Median',  'Obs.')
rownames(table_descriptives) <- c('Post',
  "Property tax p.c.","Total revenue own p.c.",'Total tax p.c.',"Revenue p.c.",
  "Population", "Log(Population)", "Neighbors with WNS",
  "Q1 avg. min. temp.", "Q2 avg. min. temp.", "Q3 avg. min. temp.", "Q4 avg. min. temp.",
  "Q1 avg. precip.", "Q2 avg. precip.", "Q3 avg. precip.", "Q4 avg. precip."
)

#----------------------------------------------------------------
# Output table to LaTeX
#----------------------------------------------------------------

custom_header <- list()
custom_header$pos <- list(-1)  # Position before the first row (column names)
custom_header$command <- c("  & \\multicolumn{4}{c}{Rural Counties} & \\multicolumn{4}{c}{Urban Counties} \\  \\\\\n")

# Aggregate descriptive table
print(xtable(table_descriptives, align = "lcccccccc"),
      type = 'latex',
      floating = F,
      sanitize.text.function = function(x){x},
      hline.after = c(-1, 0, nrow(table_descriptives)),
      booktabs = T,
      add.to.row = custom_header,
      file = '../../tables_figures_SI/table_SI-3_panel_c.tex')
