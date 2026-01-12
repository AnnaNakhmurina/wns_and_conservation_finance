#----------------------------------------------------------------
# Table SI-6 Panel A: Bond spread regressions comparing counties with high vs. low
# property assessment frequency. Estimates treatment effects separately by assessment
# frequency and tests for differential effects.
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
# Regressions on the rural subsample
#----------------------------------------------------------------

high_freq <-  feols(statetaxSPREAD_MMAinterp ~ wns
                    + cum_outbreak_count
                    + lnsize
                    + lnmat
                    + BQ
                    + GO
                    + callable
                    + insured_mergent
                    + log_pop
                    + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                    + q1precip + q2precip + q3precip +  q4precip
                    |identif + year+  agg_rating_min_lt , cluster ~ identif,
                    fixef.rm =  "singleton",
                    data = spread %>% filter( low_assesment_frequency == 0 )
)
summary(high_freq)

low_freq <-  feols(statetaxSPREAD_MMAinterp ~ wns
                   + cum_outbreak_count
                   + lnsize
                   + lnmat
                   + BQ
                   + GO
                   + callable
                   + insured_mergent
                   + log_pop
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip +  q4precip
                   |identif + year+  agg_rating_min_lt , cluster ~ identif,
                   fixef.rm =  "singleton",
                   data = spread %>% filter( low_assesment_frequency == 1 )
)
summary(low_freq)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable labels
dict <- c(statetaxSPREAD_MMAinterp = "Spread",
          lnsize = "Log(Bond Size)",
          lnmat = "Log(Maturity)",
          log_pop = "Log(Population)",
          BQ = "BQ",
          GO = "GO",
          insured_mergent = "Insured",
          year = 'Year',
          identif = "County",
          agg_rating_min_lt = "Rating",
          wns = "Post",
          callable = "Callable",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

# Count the number of unique issuers
num_identif = c( high_freq$fixef_sizes["identif"],  low_freq$fixef_sizes["identif"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(high_freq), get_mean_dependent_variable(low_freq))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(high_freq, low_freq,
       file = paste0( '../../tables_figures_SI/table_SI-6_panel_a.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       drop = c("min. temp", "precip"),
       extralines = list(
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = c( rep("Yes", 2))
       ),
       headers = list( ":_:" = list( "High " = 1, "Low " = 1 )),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
