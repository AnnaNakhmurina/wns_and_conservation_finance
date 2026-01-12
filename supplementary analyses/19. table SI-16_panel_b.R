#----------------------------------------------------------------
# Table SI-16 Panel B: Per capita property tax and own-source revenue
# regressions for rural counties. Outputs a LaTeX table.
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(fixest)
library(dplyr)
library(haven)
library(DescTools)

# Custom functions
cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}

get_mean_dependent_variable <- function(model) {
  dependent_variable <- all.vars(model$fml)[1]
  original_data <- eval(model$call$data)
  mean_dependent_variable <- mean(original_data[[dependent_variable]], na.rm = TRUE)
  return(mean_dependent_variable)
}

winsorize <- function(x, p = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, probs = p, na.rm = TRUE)
  DescTools::Winsorize(x, val = q)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta("../../../data/2024/bats_county_financial_data.dta")

# Subset to rural counties using a more strict definition of rural
data <- data %>% filter(pure_rural == 1)

# Winsorize continuous variables on this sample
data <- data %>%
  mutate_at(vars(contains(c('property_tax_per_capita', "total_rev_own_sources_per_capita", "lnpop",
                            "cum_outbreak_count",
                            "q1mintemp", "q2mintemp", "q3mintemp", "q4mintemp",
                            "q1precip", "q2precip", "q3precip", "q4precip"))), .funs = list(~winsorize(.)))

#----------------------------------------------------------------
# Per capita regressions
#----------------------------------------------------------------

property_tax_1 <- feols(property_tax_per_capita ~ wns
                      + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                      + q1precip + q2precip + q3precip + q4precip
                      | year + fips,
                      cluster ~ fips,
                      fixef.rm = "singleton",
                      data = data)
summary(property_tax_1)

property_tax <- feols(property_tax_per_capita ~ wns
              + lnpop
              + cum_outbreak_count
              + q1mintemp + q2mintemp + q3mintemp + q4mintemp
              + q1precip + q2precip + q3precip + q4precip
              | year + fips,
              cluster ~ fips,
              fixef.rm = "singleton",
              data = data)
summary(property_tax)

revenue_own_1 <- feols(total_rev_own_sources_per_capita ~ wns
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip + q4precip
                     | year + fips,
                     cluster ~ fips,
                     fixef.rm = "singleton",
                     data = data)
summary(revenue_own_1)

revenue_own <- feols(total_rev_own_sources_per_capita ~ wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip + q4precip
                     | year + fips,
                     cluster ~ fips,
                     fixef.rm = "singleton",
                     data = data)
summary(revenue_own)

# Set dictionary
dict <- c(property_tax_per_capita = "Property tax p.c.",
          total_rev_own_sources_per_capita = "Total revenue own sources p.c.",
          wns = "Post",
          year = "Year",
          fips = "County",
          lnpop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp')

# Count the number of unique counties
num_identif <- c(property_tax_1$fixef_sizes["fips"], property_tax$fixef_sizes["fips"],
                 revenue_own_1$fixef_sizes["fips"], revenue_own$fixef_sizes["fips"])
names(num_identif) <- NULL
num_identif <- cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c(get_mean_dependent_variable(property_tax_1), get_mean_dependent_variable(property_tax),
                             get_mean_dependent_variable(revenue_own_1), get_mean_dependent_variable(revenue_own))
mean_dependent_variable <- cust_format1(mean_dependent_variable, 2)

etable(property_tax_1, property_tax,
       revenue_own_1, revenue_own,
       file = paste0('../../tables_figures_SI/table_SI-16_panel_b.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats = "r3", float = FALSE, coefstat = "se", fitstat = ~ ar2 + n,
       keep = c("Post", "Log Population", "Neighbors with WNS"),
       extralines = list(
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = rep("Yes", 4)
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
