#----------------------------------------------------------------
# Table SI-12: Robustness checks for fiscal regressions. Panel A uses aggregate
# measures (total tax and total revenue per capita). Panel B restricts the sample
# to counties that also appear in the bond spread data.
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(fixest)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  library(data.table)
  library(haven)
}

# Custom formatting functions
cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}

get_mean_dependent_variable <- function(model) {
  dependent_variable <- all.vars(model$fml)[1]
  original_data <- eval(model$call$data)
  mean_dependent_variable <- mean(original_data[[dependent_variable]], na.rm = TRUE)
  return(mean_dependent_variable)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta( "../../../data/2024/bats_rural_financial_data.dta")

# Load bond data to restrict the analyses to the subset of counties with bond data
spread <- read_dta("../../../data/2024/bats_rural_spread_data.dta")

# Variable labels
dict <- c(property_tax_per_capita = "Property tax p.c.",
          total_tax_per_capita = 'Total tax p.c.',
          total_revenue_per_capita = "Revenue p.c.",
          total_rev_own_sources_per_capita = "Total revenue own sources p.c.",
          wns = "Post",
          year = "Year",
          fips = "County",
          lnpop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

#----------------------------------------------------------------
# Aggregate tax and revenue regressions
#----------------------------------------------------------------

total_tax_1 <- feols(total_tax_per_capita ~  wns
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip +  q4precip
                   | year + fips  , 
                   cluster ~ fips, 
                   fixef.rm =  "singleton",
                   data = data
)
summary(total_tax_1)

total_tax <- feols(total_tax_per_capita ~  wns
                   + lnpop
                   + cum_outbreak_count
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip +  q4precip
                   | year + fips  , 
                   cluster ~ fips, 
                   fixef.rm =  "singleton",
                   data = data
)
summary(total_tax)

revenue_1 <- feols(total_revenue_per_capita ~  wns
                 + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                 + q1precip + q2precip + q3precip +  q4precip
                 | year + fips  , 
                 cluster ~ fips,
                 fixef.rm =  "singleton",
                 data = data
)
summary(revenue_1)

revenue <- feols(total_revenue_per_capita ~  wns
                 + lnpop
                 + cum_outbreak_count
                 + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                 + q1precip + q2precip + q3precip +  q4precip
                 | year + fips  ,
                 cluster ~ fips,
                 fixef.rm =  "singleton",
                 data = data
)
summary(revenue)

#----------------------------------------------------------------
# Print to table - Panel A
#----------------------------------------------------------------

# Count the number of unique issuers
num_identif = c( total_tax_1$fixef_sizes["fips"],  total_tax$fixef_sizes["fips"],
                 revenue_1$fixef_sizes["fips"],  revenue$fixef_sizes["fips"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(total_tax_1), get_mean_dependent_variable(total_tax),
                               get_mean_dependent_variable(revenue_1), get_mean_dependent_variable(revenue))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(total_tax_1, total_tax,
       revenue_1, revenue,
       file = paste0( '../../tables_figures_SI/table_SI-12_panel_a.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
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

#----------------------------------------------------------------
# Regressions restricted to counties with bond data
#----------------------------------------------------------------

property_tax_1 <- feols(property_tax_per_capita ~  wns
                        + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                        + q1precip + q2precip + q3precip +  q4precip
                        | year + fips  , 
                        cluster ~ fips, 
                        fixef.rm =  "singleton",
                        data = data %>% filter( fips %in% spread$fips)
)
summary(property_tax_1)

property_tax <- feols(property_tax_per_capita ~  wns
                      + lnpop
                      + cum_outbreak_count
                      + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                      + q1precip + q2precip + q3precip +  q4precip
                      | year + fips  ,
                      cluster ~ fips,
                      fixef.rm =  "singleton",
                      data = data  %>% filter( fips %in% spread$fips)
)
summary(property_tax)

revenue_own_1 <- feols(total_rev_own_sources_per_capita ~  wns
                       + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                       + q1precip + q2precip + q3precip +  q4precip
                       | year + fips  , 
                       cluster ~ fips,
                       fixef.rm =  "singleton",
                       data = data  %>% filter( fips %in% spread$fips)
)
summary(revenue_own_1)

revenue_own <- feols(total_rev_own_sources_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  , 
                     cluster ~ fips, 
                     fixef.rm =  "singleton",
                     data = data %>% filter( fips %in% spread$fips)
)
summary(revenue_own)

#----------------------------------------------------------------
# Print to table - Panel B
#----------------------------------------------------------------

# Count the number of unique issuers
num_identif = c( property_tax_1$fixef_sizes["fips"],  property_tax$fixef_sizes["fips"],
                 revenue_own_1$fixef_sizes["fips"],  revenue_own$fixef_sizes["fips"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(property_tax_1), get_mean_dependent_variable(property_tax),
                               get_mean_dependent_variable(revenue_own_1), get_mean_dependent_variable(revenue_own))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(property_tax_1, property_tax,
       revenue_own_1, revenue_own,
       file = paste0( '../../tables_figures_SI/table_SI-12_panel_b.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
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
