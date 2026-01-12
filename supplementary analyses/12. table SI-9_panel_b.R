#----------------------------------------------------------------
# Table SI-9 Panel B: Per capita revenue regressions with alternative weather
# controls. Compares specifications using quadratic minimum temperatures vs.
# temperature bins for property tax and own-source revenue outcomes.
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
# Per capita regressions with squared temps
#----------------------------------------------------------------

property_tax_sq <- feols(property_tax_per_capita ~  wns
              + lnpop
              + cum_outbreak_count
              + q1mintemp + q2mintemp + q3mintemp + q4mintemp
              + q1mintemp_sq + q2mintemp_sq + q3mintemp_sq + q4mintemp_sq
              + q1precip + q2precip + q3precip +  q4precip
              | year + fips  , # FE
              cluster ~ fips, # Clustering
              fixef.rm =  "singleton",
              data = data
)
summary(property_tax_sq)

revenue_own_sq <- feols(total_rev_own_sources_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1mintemp_sq + q2mintemp_sq + q3mintemp_sq + q4mintemp_sq
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  , # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data
)
summary(revenue_own_sq)

#----------------------------------------------------------------
# Regressions with temp bins
#----------------------------------------------------------------

property_tax_bin <- feols(property_tax_per_capita ~  wns
                         + lnpop
                         + cum_outbreak_count
                         + tminlt10_q1 + tminlt10_q2 + tminlt10_q3 + tminlt10_q4
                         + tmin1020_q1 + tmin1020_q2 + tmin1020_q3 + tmin1020_q4
                         + tmin2030_q1 + tmin2030_q2 + tmin2030_q3 + tmin2030_q4
                         + tmingt30_q1 + tmingt30_q2 + tmingt30_q3 + tmingt30_q4
                         + tmaxlt10_q1 + tmaxlt10_q2 + tmaxlt10_q3 + tmaxlt10_q4
                         + tmax1020_q1 + tmax1020_q2 + tmax1020_q3 + tmax1020_q4
                         + tmax2030_q1 + tmax2030_q2 + tmax2030_q3 + tmax2030_q4
                         + tmaxgt30_q1 + tmaxgt30_q2 + tmaxgt30_q3 + tmaxgt30_q4
                         + tavglt10_q1 + tavglt10_q2 + tavglt10_q3 + tavglt10_q4
                         + tavg1020_q1 + tavg1020_q2 + tavg1020_q3 + tavg1020_q4
                         + tavg2030_q1 + tavg2030_q2 + tavg2030_q3 + tavg2030_q4
                         + tavggt30_q1 + tavggt30_q2 + tavggt30_q3 + tavggt30_q4
                         + q1precip + q2precip + q3precip +  q4precip
                         | year + fips  , # FE
                         cluster ~ fips, # Clustering
                         fixef.rm =  "singleton",
                         data = data
)
summary(property_tax_bin)

revenue_own_bin <- feols(total_rev_own_sources_per_capita ~  wns
                        + lnpop
                        + cum_outbreak_count
                        + tminlt10_q1 + tminlt10_q2 + tminlt10_q3 + tminlt10_q4
                        + tmin1020_q1 + tmin1020_q2 + tmin1020_q3 + tmin1020_q4
                        + tmin2030_q1 + tmin2030_q2 + tmin2030_q3 + tmin2030_q4
                        + tmingt30_q1 + tmingt30_q2 + tmingt30_q3 + tmingt30_q4
                        + tmaxlt10_q1 + tmaxlt10_q2 + tmaxlt10_q3 + tmaxlt10_q4
                        + tmax1020_q1 + tmax1020_q2 + tmax1020_q3 + tmax1020_q4
                        + tmax2030_q1 + tmax2030_q2 + tmax2030_q3 + tmax2030_q4
                        + tmaxgt30_q1 + tmaxgt30_q2 + tmaxgt30_q3 + tmaxgt30_q4
                        + tavglt10_q1 + tavglt10_q2 + tavglt10_q3 + tavglt10_q4
                        + tavg1020_q1 + tavg1020_q2 + tavg1020_q3 + tavg1020_q4
                        + tavg2030_q1 + tavg2030_q2 + tavg2030_q3 + tavg2030_q4
                        + tavggt30_q1 + tavggt30_q2 + tavggt30_q3 + tavggt30_q4
                        + q1precip + q2precip + q3precip +  q4precip
                        | year + fips  , # FE
                        cluster ~ fips, # Clustering
                        fixef.rm =  "singleton",
                        data = data
)
summary(revenue_own_bin)

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
num_identif = c(  property_tax_bin$fixef_sizes["fips"], property_tax_sq$fixef_sizes["fips"],
                   revenue_own_bin$fixef_sizes["fips"], revenue_own_sq$fixef_sizes["fips"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c (  get_mean_dependent_variable(property_tax_bin), get_mean_dependent_variable(property_tax_sq),
                               get_mean_dependent_variable(revenue_own_bin), get_mean_dependent_variable(revenue_own_sq))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(
       property_tax_bin, property_tax_sq, revenue_own_bin, revenue_own_sq,
       file = paste0( '../../tables_figures_SI/table_SI-9_panel_b.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       drop = c("min. temp", "precip", "mintemp", "tmin", "tmax", "tavg"),
       extralines = list(
         '-^Quarterly temp. bins' = c( "Yes", "No", "Yes", "No"),
         '-^Squared quarterly temp.' = c( "No", "Yes",  "No", "Yes")
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
