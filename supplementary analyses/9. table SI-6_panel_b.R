#----------------------------------------------------------------
# Table SI-6 Panel B: Per capita revenue regressions comparing counties with high
# vs. low property assessment frequency. Estimates effects on property tax and
# total own-source revenue separately by assessment frequency.
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
# Per capita regressions
#----------------------------------------------------------------

property_tax_high <- feols(property_tax_per_capita ~  wns
              + lnpop
              + cum_outbreak_count
              + q1mintemp + q2mintemp + q3mintemp + q4mintemp
              + q1precip + q2precip + q3precip +  q4precip
              | year + fips  , # FE
              cluster ~ fips, # Clustering
              fixef.rm =  "singleton",
              data = data %>% filter( low_assesment_frequency == 0 )
)
summary(property_tax_high)

property_tax_low <- feols(property_tax_per_capita ~  wns
                           + lnpop
                           + cum_outbreak_count
                           + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                           + q1precip + q2precip + q3precip +  q4precip
                           | year + fips  , # FE
                           cluster ~ fips, # Clustering
                           fixef.rm =  "singleton",
                           data = data %>% filter( low_assesment_frequency == 1 )
)
summary(property_tax_low)

revenue_own_high <- feols(total_rev_own_sources_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  , # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data %>% filter( low_assesment_frequency == 0 )
)
summary(revenue_own_high)

revenue_own_low <- feols(total_rev_own_sources_per_capita ~  wns
                          + lnpop
                          + cum_outbreak_count
                          + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                          + q1precip + q2precip + q3precip +  q4precip
                          | year + fips  , # FE
                          cluster ~ fips, # Clustering
                          fixef.rm =  "singleton",
                          data = data %>% filter( low_assesment_frequency == 1 )
)
summary(revenue_own_low)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable labels
dict <- c(property_tax_per_capita = "Property tax p.c.",
          total_rev_own_sources_per_capita = "Total revenue own sources p.c.",
          wns = "Post",
          year = "Year",
          fips = "County",
          lnpop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

# Count the number of unique issuers
num_identif = c( property_tax_high$fixef_sizes["fips"],  property_tax_low$fixef_sizes["fips"],
                 revenue_own_high$fixef_sizes["fips"],  revenue_own_low$fixef_sizes["fips"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(property_tax_high), get_mean_dependent_variable(property_tax_low),
                               get_mean_dependent_variable(revenue_own_high), get_mean_dependent_variable(revenue_own_low))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(property_tax_high, property_tax_low, revenue_own_high, revenue_own_low,
       file = paste0( '../../tables_figures_SI/table_SI-6_panel_b.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       drop = c("min. temp", "precip"),
       extralines = list(
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = c( rep("Yes", 4))
       ),
       headers = list( ":_:" = list( "High " = 1, "Low " = 1, "High " = 1, "Low " = 1 )),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
