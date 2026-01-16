#----------------------------------------------------------------
# Table SI-9 Panel A: Bond spread regressions with alternative weather controls.
# Compares specifications using quadratic minimum temperatures vs. temperature bins.
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

spread <- read_dta(paste0("../../../data/2024/bats_rural_spread_data.dta"))

#----------------------------------------------------------------
# Regressions with quadratic min temps
#----------------------------------------------------------------

m1 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             + lnsize
             + lnmat
             + BQ
             + GO
             + callable
             + insured_mergent
             + log_pop
             + cum_outbreak_count
             + q1mintemp + q2mintemp + q3mintemp + q4mintemp
             + q1mintemp_sq + q2mintemp_sq + q3mintemp_sq + q4mintemp_sq
             + q1precip + q2precip + q3precip +  q4precip
             |identif + year+  agg_rating_min_lt , cluster ~ identif,
             fixef.rm =  "singleton",
             data = spread
)
summary(m1)

#----------------------------------------------------------------
# Regressions with temp bins
#----------------------------------------------------------------

m2 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             + lnsize
             + lnmat
             + BQ
             + GO
             + callable
             + insured_mergent
             + log_pop
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
             |identif + year+  agg_rating_min_lt , cluster ~ identif,
             fixef.rm =  "singleton",
             data = spread
)
summary(m2)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable labels
dict <- c(statetaxSPREAD_MMAinterp = "Spread",
          lnsize = "Log(Bond Size)",
          lnmat = "Log(Maturity)",
          BQ = "BQ",
          GO = "GO",
          insured_mergent = "Insured",
          year = 'Year',
          identif = "County",
          agg_rating_min_lt = "Rating",
          wns = "Post",
          callable = "Callable",
          log_pop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

# Count the number of unique issuers
num_identif = c(  m1$fixef_sizes["identif"], m2$fixef_sizes["identif"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(m1), get_mean_dependent_variable(m2))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable( m1, m2,
        file = paste0( '../../tables_figures_SI/table_SI-9_panel_a.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       drop = c("min. temp", "precip", "mintemp", "tmin", "tmax", "tavg"),
       extralines = list(
         '-^Quarterly temp. bins' = c( "Yes", "No"),
         '-^Squared quarterly temp.' = c( "No", "Yes"),
         '-^Linear quarterly temp.' = c( "No", "Yes")
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
