
#----------------------------------------------------------------
# Table SI-4 panel A: Bond spread regressions for rural counties
#
# This script runs fixed effects regressions of municipal bond spreads
# on WNS exposure for rural counties, with progressively richer controls
# (weather, bond characteristics, ratings, population).
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

spread <- read_dta(paste0("../../../data/2024/bats_rural_spread_data.dta"))

#----------------------------------------------------------------
# Regressions
#----------------------------------------------------------------

m1 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             |identif + year , cluster ~ identif,
             data = spread
)
summary(m1)

m2 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             + cum_outbreak_count
             + q1mintemp + q2mintemp + q3mintemp + q4mintemp
             + q1precip + q2precip + q3precip +  q4precip
             |identif + year , cluster ~ identif,
             data = spread
)
summary(m2)

m3 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + lnmat
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip +  q4precip
            |identif + year , cluster ~ identif,
            data = spread
            )
summary(m3)

m4 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + lnmat
            + BQ
            + GO
            + callable
            + insured_mergent
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip +  q4precip
            |identif + year , cluster ~ identif,
             fixef.rm =  "singleton",
            data = spread
            )
summary(m4)

m5 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             + lnsize
             + lnmat
             + BQ
             + GO
             + callable
             + insured_mergent
             + cum_outbreak_count
             + q1mintemp + q2mintemp + q3mintemp + q4mintemp
             + q1precip + q2precip + q3precip +  q4precip
             |identif + year+  agg_rating_min_lt , cluster ~ identif,
             fixef.rm =  "singleton",
             data = spread
)
summary(m5)

m6 <-  feols(statetaxSPREAD_MMAinterp ~ wns
             + lnsize
             + lnmat
             + BQ
             + GO
             + callable
             + insured_mergent
             + log_pop
             + cum_outbreak_count
             + q1mintemp + q2mintemp + q3mintemp + q4mintemp
             + q1precip + q2precip + q3precip +  q4precip
             |identif + year+  agg_rating_min_lt , cluster ~ identif,
             fixef.rm =  "singleton",
             data = spread
)
summary(m6)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Set dictionary
dict<- c(statetaxSPREAD_MMAinterp = "Spread",
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
num_identif = c( m1$fixef_sizes["identif"],  m2$fixef_sizes["identif"],
                 m3$fixef_sizes["identif"], m4$fixef_sizes["identif"],  m5$fixef_sizes["identif"], m6$fixef_sizes["identif"] )
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(m1), get_mean_dependent_variable(m2),
                               get_mean_dependent_variable(m3),get_mean_dependent_variable(m4), get_mean_dependent_variable(m5),
                               get_mean_dependent_variable(m6))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable(m1, m2, m3, m4, m5, m6,
       file = paste0( '../../tables_figures_SI/table_SI-4_panel_a.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       drop = c("min. temp", "precip"),
       extralines = list(
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = c( "No", rep("Yes", 5))
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
