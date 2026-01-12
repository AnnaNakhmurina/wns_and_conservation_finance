
#----------------------------------------------------------------
# Table SI-5 panel A: Bond spread regressions by bat population
#
# This script runs fixed effects regressions of municipal bond spreads
# on WNS exposure, separately for counties with high vs low bat populations.
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(fixest)
  library(dplyr)
  library(data.table)
  library(haven)
}

#----------------------------------------------------------------
# Custom functions
#----------------------------------------------------------------

cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}

get_mean_dependent_variable <- function(model) {
  # Extract the dependent variable name
  dependent_variable <- all.vars(model$fml)[1]

  # Evaluate the call to get the original data
  original_data <- eval(model$call$data)

  # Calculate the mean of the dependent variable
  mean_dependent_variable <- mean(original_data[[dependent_variable]], na.rm = TRUE)

  # Return the mean
  return(mean_dependent_variable)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

spread <- read_dta( "../../../data/2024/bats_rural_spread_data.dta")

#----------------------------------------------------------------
# Regressions on the rural subsample
#----------------------------------------------------------------

high_bats <-  feols(statetaxSPREAD_MMAinterp ~ wns
                    + cum_outbreak_count
                    + lnsize
                    + lnmat
                    + BQ
                    + GO
                    + callable
                    + log_pop
                    + insured_mergent
                    + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                    + q1precip + q2precip + q3precip +  q4precip
                    |identif + year+  agg_rating_min_lt , cluster ~ identif,
                    fixef.rm =  "singleton",
                    data = spread %>% filter( high_bats == 1 )
)
summary(high_bats)

low_bats <-  feols(statetaxSPREAD_MMAinterp ~ wns
                   + cum_outbreak_count
                   + lnsize
                   + lnmat
                   + BQ
                   + GO
                   + callable
                   + log_pop
                   + insured_mergent
                   + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                   + q1precip + q2precip + q3precip +  q4precip
                   |identif + year+  agg_rating_min_lt , cluster ~ identif,
                   fixef.rm =  "singleton",
                   data = spread %>% filter( high_bats == 0 )
)
summary(low_bats)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Set dictionary
dict<- c(statetaxSPREAD_MMAinterp = "Spread",
         lnsize = "Log(Bond Size)",
         lnmat = "Log(Maturity)",
         BQ = "BQ",
         GO = "GO",
         log_pop = "Log(Population)",
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
num_identif = c( high_bats$fixef_sizes["identif"],  low_bats$fixef_sizes["identif"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(high_bats), get_mean_dependent_variable(low_bats))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(high_bats, low_bats,
       file = paste0( '../../tables_figures_SI/table_SI-5_panel_a.tex'),
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
