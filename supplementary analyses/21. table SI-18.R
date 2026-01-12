#----------------------------------------------------------------
# Table SI-18: Secondary market bond spread regressions for rural counties
# Builds up specification by progressively adding bond and county controls
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(fixest)
  library(dplyr)
  library(haven)
}

# Custom functions
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

spread <- read_dta(paste0("../../../data/2024/bats_county_issuers_secondary_rural.dta"))

#----------------------------------------------------------------
# Regressions
#----------------------------------------------------------------

m1 <- feols(statetaxSPREAD_MMAinterp ~ wns
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m1)

m2 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m2)

# Add bond size and maturity

m3 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + log_time_to_maturity
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m3)

m4 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + log_time_to_maturity
            + BQ
            + GO
            + callable
            + tax_exempt
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m4)

m5 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + log_time_to_maturity
            + BQ
            + GO
            + callable
            + insured_mergent_adjusted
            + tax_exempt
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m5)

m6 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + log_time_to_maturity
            + BQ
            + GO
            + callable
            + insured_mergent_adjusted
            + tax_exempt
            + log_pop
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread
)
summary(m6)

m7 <- feols(statetaxSPREAD_MMAinterp ~ wns
            + lnsize
            + log_time_to_maturity
            + BQ
            + GO
            + callable
            + insured_mergent_adjusted
            + tax_exempt
            + log_pop
            + cum_outbreak_count
            + q1mintemp + q2mintemp + q3mintemp + q4mintemp
            + q1precip + q2precip + q3precip + q4precip
            | identif + year + month + rating_average_under, cluster ~ identif,
            fixef.rm = "singleton",
            data = spread %>% filter(tax_exempt == 1)
)
summary(m7)

etable(m1, m2, m3, m4, m5, m6)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable dictionary
dict <- c(statetaxSPREAD_MMAinterp = "Spread",
          lnsize = "Log(Bond Size)",
          log_time_to_maturity = "Log(Maturity)",
          BQ = "BQ",
          GO = "GO",
          insured_mergent_adjusted = "Insured",
          year = 'Year',
          identif = "County",
          tax_exempt = "Tax Exempt",
          month = "Trade Month",
          rating_average_under = "Rating",
          wns = "Post",
          callable = "Callable",
          log_pop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp'
)

# Count the number of unique issuers:
num_identif = c(m1$fixef_sizes["identif"], m2$fixef_sizes["identif"],
                m3$fixef_sizes["identif"], m4$fixef_sizes["identif"],
                m5$fixef_sizes["identif"], m6$fixef_sizes["identif"],
                m7$fixef_sizes["identif"])
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c(get_mean_dependent_variable(m1), get_mean_dependent_variable(m2),
                             get_mean_dependent_variable(m3), get_mean_dependent_variable(m4),
                             get_mean_dependent_variable(m5), get_mean_dependent_variable(m6),
                             get_mean_dependent_variable(m7))
mean_dependent_variable = cust_format1(mean_dependent_variable, 2)

etable(m1, m2, m3, m4, m5, m6, m7,
       file = paste0('../../tables_figures_SI/table_SI-18.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats = "r3", float = FALSE, coefstat = "se", fitstat = ~ ar2 + n,
       drop = c("min. temp", "precip"),
       extralines = list(
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = c("No", rep("Yes", 6)),
         '-^Tax-exempt sample' = c(rep("No", 6), "Yes")
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
