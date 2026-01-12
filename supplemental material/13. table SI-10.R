#----------------------------------------------------------------
# Table SI-10: Agricultural property assessment regressions. Panel A examines
# assessed taxes per capita with various sample restrictions. Panel B examines
# alternative outcomes (log taxes, log taxes per acre, log acres).
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

data <- read_dta( "../../../data/2024/assesments_rural.dta")

#----------------------------------------------------------------
# Per capita regressions
#----------------------------------------------------------------

per_capita <- feols(total_tax_amount_per_capita ~  wns
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  + land_use_code, # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data
)
summary(per_capita)

per_capita_full <- feols(total_tax_amount_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  + land_use_code , # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data
)
summary(per_capita_full)

per_capita_post2015 <- feols(total_tax_amount_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  + land_use_code , # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data %>% filter( year > 2015 )
)
summary(per_capita_post2015)

per_capita_luse_bigger500 <- feols(total_tax_amount_per_capita ~  wns
                     + lnpop
                     + cum_outbreak_count
                     + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                     + q1precip + q2precip + q3precip +  q4precip
                     | year + fips  + land_use_code , # FE
                     cluster ~ fips, # Clustering
                     fixef.rm =  "singleton",
                     data = data %>% filter( land_use_code >= 500 )
)
summary(per_capita_luse_bigger500)

#----------------------------------------------------------------
# Alternative outcome regressions
#----------------------------------------------------------------

log_tax_amount <- feols(log_tax_amount ~  wns
               + lnpop
               + cum_outbreak_count
               + q1mintemp + q2mintemp + q3mintemp + q4mintemp
               + q1precip + q2precip + q3precip +  q4precip
               | year + fips + land_use_code  , # FE
               cluster ~ fips, # Clustering
               fixef.rm =  "singleton",
               data = data
)
summary(log_tax_amount)

per_cropland <- feols(log_tax_per_cropland ~  wns
                        + lnpop
                        + cum_outbreak_count
                        + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                        + q1precip + q2precip + q3precip +  q4precip
                        | year + fips + land_use_code  , # FE
                        cluster ~ fips, # Clustering
                        fixef.rm =  "singleton",
                        data = data
)
summary(per_cropland)

acres <- feols( log_total_acres ~  wns
                      + lnpop
                      + cum_outbreak_count
                      + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                      + q1precip + q2precip + q3precip +  q4precip
                      | year + fips + land_use_code  , # FE
                      cluster ~ fips, # Clustering
                      fixef.rm =  "singleton",
                      data = data
)
summary(acres)

#----------------------------------------------------------------
# Print to table - Panel A
#----------------------------------------------------------------

# Variable labels
dict <- c(total_tax_amount_per_capita = "Assessed taxes p.c.",
          log_tax_amount = 'Log(Assessed taxes)',
          log_tax_per_cropland = 'Log(Assessed tax p.a.)',
          log_total_acres = 'Log(Acres)',
          wns = "Post",
          year = "Year",
          fips = "County",
          lnpop = "Log Population",
          cum_outbreak_count = "Neighbors with WNS",
          q1precip = 'Q1 avg. precip', q2precip = 'Q2 avg. precip', q3precip = 'Q3 avg. precip', q4precip = 'Q4 avg. precip',
          q1mintemp = 'Q1 avg. min. temp', q2mintemp = 'Q2 avg. min. temp', q3mintemp = 'Q3 avg. min. temp', q4mintemp = 'Q4 avg. min. temp',
          land_use_code = 'Land use code'
)

# Count the number of unique issuers
num_identif = c( per_capita$fixef_sizes["fips"],  per_capita_full$fixef_sizes["fips"],
                 per_capita_post2015$fixef_sizes["fips"],  per_capita_luse_bigger500$fixef_sizes["fips"])
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c ( get_mean_dependent_variable(per_capita), get_mean_dependent_variable(per_capita_full),
                               get_mean_dependent_variable(per_capita_post2015), get_mean_dependent_variable(per_capita_luse_bigger500))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable( per_capita, per_capita_full, per_capita_post2015, per_capita_luse_bigger500,
        file = paste0( '../../tables_figures_SI/table_SI-10_panel_a.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       keep = c("Post", "Log Population", "Neighbors with WNS"),
       extralines = list(
         '-^Post-2016 sample' = c(rep("No", 2), 'Yes', rep("No", 1)),
         '-^Land use more likely ag.' = c(rep("No", 3), 'Yes'),
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
# Print to table - Panel B
#----------------------------------------------------------------

num_identif = c(
                 log_tax_amount$fixef_sizes["fips"], per_cropland$fixef_sizes["fips"], acres$fixef_sizes["fips"])
names(num_identif) = NULL
num_identif = cust_format2(num_identif, 0)

# Mean dependent variable for each specification
mean_dependent_variable <- c (
                               get_mean_dependent_variable(log_tax_amount), get_mean_dependent_variable(per_cropland) , get_mean_dependent_variable(acres))
mean_dependent_variable = cust_format1( mean_dependent_variable, 2)

etable( log_tax_amount, per_cropland, acres,
        file = paste0( '../../tables_figures_SI/table_SI-10_panel_b.tex'),
       replace = T,
       dict = dict, digits = "r3", digits.stats  = "r3", float=FALSE, coefstat= "se", fitstat = ~ ar2 + n ,
       keep = c("Post", "Log Population", "Neighbors with WNS"),
       extralines = list(
         '-^Post-2016 sample' = c(rep("No", 3)),
         '-^Land use more likely ag.' = c(rep("No", 3)),
         '-^Number of counties' = num_identif,
         '-^Mean dependent variable' = mean_dependent_variable,
         '-^Weather controls' = rep("Yes", 3)
       ),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
