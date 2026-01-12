#----------------------------------------------------------------
# Table SI-17 Panel A: Relationship between bat occurrence and WNS susceptibility
# Tests whether big brown bat occurrence correlates with WNS-susceptible species counts
#----------------------------------------------------------------

rm(list=ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(fixest)
  library(haven)
}

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta( "../../../data/2024/natbat_clean.dta")

#----------------------------------------------------------------
# Regress change in occurence on susceptibility to WNS
#----------------------------------------------------------------

continuous <- feols(EPFU_mean ~  susceptible_to_WNS
              | 0 , 
              data = data
)
summary(continuous)

dummy <- feols(EPFU_mean ~  high_bats
              | 0 , 
              data = data
)
summary(dummy)

#----------------------------------------------------------------
# Print to table
#----------------------------------------------------------------

# Variable dictionary
dict <- c(EPFU_mean = "Big Brown Bat Occurence",
          high_bats = 'High Bats at Risk',
          susceptible_to_WNS = "N Species Susceptible to WNS",
          "(Intercept)" = "Constant"
)

etable(continuous, dummy,
       file = paste0('../../tables_figures_SI/table_SI-17_panel_a.tex'),
       replace = TRUE,
       dict = dict,
       digits = "r3",
       digits.stats = "r3",
       float = FALSE,
       coefstat = "se",
       fitstat = ~ ar2 + n,
       order = c("N Species", "High Bats", "Constant"),
       style.tex = style.tex("aer",
                             yesNo = c('Yes', ''),
                             fixef.title = "\\midrule",
                             fixef.where = 'var',
                             stats.title = "\\midrule",
                             tabular = "*"))
