#----------------------------------------------------------------
# Table SI-11: Revenue per cropland acre regressions. Estimates treatment effects
# on log property tax per cropland and log total own-source revenue per cropland
# with progressive addition of controls.
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

#----------------------------------------------------------------
# Per cropland regressions
#----------------------------------------------------------------

property_tax_1 <- feols( log_property_tax_per_cropland ~  wns
                      + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                      + q1precip + q2precip + q3precip +  q4precip
                      | year + fips, 
                      cluster ~ fips, 
                      fixef.rm =  "singleton",
                      data = data
)
summary(property_tax_1)

property_tax <- feols(log_property_tax_per_cropland ~  wns
              + lnpop
              + cum_outbreak_count
              + q1mintemp + q2mintemp + q3mintemp + q4mintemp
              + q1precip + q2precip + q3precip +  q4precip
              | year + fips  , 
              cluster ~ fips,
              fixef.rm =  "singleton",
              data = data
)
summary(property_tax)

revenue_own_1 <- feols(log_total_rev_own_per_cropland ~  wns
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  , 
                     cluster ~ fips, 
                     fixef.rm =  "singleton",
                     data = data
)
summary(revenue_own_1)

revenue_own <- feols(log_total_rev_own_per_cropland ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  , 
                     cluster ~ fips,
                     fixef.rm =  "singleton",
                     data = data
)
summary(revenue_own)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable labels
dict <- c(log_property_tax_per_cropland = "Log(Property tax per cropland)",
          log_total_rev_own_per_cropland = "Log(Total revenue own sources per cropland)",
          wns = "Post",
          year = "Year",
          fips = "County",
          lnpop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

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
       file = paste0( '../../tables_figures_SI/table_SI-11.tex'),
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
