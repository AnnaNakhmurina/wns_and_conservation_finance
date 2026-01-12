#----------------------------------------------------------------
# Table SI-14 Panel A: Bond spread regressions restricted to states with WNS.
# Progressively adds controls (weather, bond characteristics, ratings, population)
# to estimate treatment effects on municipal bond spreads.
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
  library(DescTools)
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

winsorize <- function(x, p = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(x)
  q <- quantile(x, probs = p, na.rm = TRUE)
  DescTools::Winsorize(x, val = q)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

spread <- read_dta("../../../data/2024/bats_rural_spread_data.dta")

# Restrict to states with WNS
wns_states = spread %>% filter( wns !=0 ) %>% dplyr::select(STATEFP) %>% distinct()
spread = spread %>% filter( STATEFP %in% wns_states$STATEFP )

# Winsorize continuous variables on this sample
spread <- spread %>%
  mutate_at(vars(contains(c('spread', "total_maturity_offering_amt_f", "size", "lnmat", "lnsize",
                            "cum_outbreak_count",
                            "q1mintemp", "q2mintemp", "q3mintemp", "q4mintemp",
                            "q1precip", "q2precip", "q3precip", "q4precip")) ), .funs = list(~winsorize(.)))

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

## Add bond size and maturity ##

m3 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + lnmat
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip +  q4precip
            |identif + year, cluster ~ identif,
            data = spread
            )
summary(m3)

## Add more bond variables ##

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
            |identif + year, cluster ~ identif,
             fixef.rm =  "singleton",
            data = spread
            )
summary(m4)

## Add rating fixed effects ##

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

## Add population control ##

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
       file = paste0( '../../tables_figures_SI/table_SI-14_panel_a.tex'),
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
