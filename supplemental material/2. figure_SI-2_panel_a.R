#----------------------------------------------------------------
# Figure SI-2 Panel A: Event study robustness checks for bond spreads
#
# This script estimates the effect of WNS on municipal bond spreads using four
# different specifications: (1) no controls, (2) full controls, (3) stacked DiD
# following Cengiz et al. (2019), and (4) imputation estimator following
# Borusyak et al. (2024). The results are combined into a single event study figure
#----------------------------------------------------------------

rm(list = ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

{
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(fixest)
  library(did)
  library(didimputation)
  library(showtext)
  library(fect)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

spread <- read_dta(paste0("../../../data/2024/bats_rural_spread_data.dta"))

#----------------------------------------------------------------
# Regression with no controls
#----------------------------------------------------------------

plot_no_controls <- feols(statetaxSPREAD_MMAinterp ~  i(modified_event_year2, 0)
                          | fips + year , cluster ~ fips + year,
                          fixef.rm =  "singleton",
                          data = spread
)
summary(plot_no_controls)

coefficients <- coef(plot_no_controls)
se <- se(plot_no_controls)
conf_intervals <- confint(plot_no_controls, level = 0.90)
result_df_no_controls <- data.frame(
  Estimate = coefficients,
  SE = se,
  Lower_CI = conf_intervals[, 1],
  Upper_CI = conf_intervals[, 2]
)
result_df_no_controls = result_df_no_controls[1:12,]
zero_df = data.frame( Estimate = 0, SE = 0, Lower_CI = 0, Upper_CI = 0)
result_df_no_controls = rbind(zero_df, result_df_no_controls)
result_df_no_controls$term = c(0, -6:-1, 1:6)
result_df_no_controls$Model = "No controls"

#----------------------------------------------------------------
# Main specification with controls
#----------------------------------------------------------------

plot <- feols(statetaxSPREAD_MMAinterp ~  i(event_year, 0)
              + lnsize
              + lnmat
              + BQ
              + GO
              + log_pop
              + callable
              + insured_mergent
              + cum_outbreak_count
              + q1mintemp + q2mintemp + q3mintemp + q4mintemp
              + q1precip + q2precip + q3precip +  q4precip
              | fips + year + agg_rating_min_lt , cluster ~ fips + year,
              fixef.rm =  "singleton",
              data = spread
)
summary(plot)

coefficients <- coef(plot)
se <- se(plot)
conf_intervals <- confint(plot, level = 0.90)
result_df <- data.frame(
  Estimate = coefficients,
  SE = se,
  Lower_CI = conf_intervals[, 1],
  Upper_CI = conf_intervals[, 2]
)
result_df = result_df[1:12,]
zero_df = data.frame( Estimate = 0, SE = 0, Lower_CI = 0, Upper_CI = 0)
result_df = rbind(zero_df, result_df)
result_df$term = c(0, -6:-1, 1:6)
result_df$Model = "With full controls"

#----------------------------------------------------------------
# Stacked specification following Cengiz et al. (2019)
#----------------------------------------------------------------

spread <- get.cohort(spread,D="wns",index=c("cusip_c","year"),start0 = TRUE)

spread_stacked <- NULL
target.cohorts <- setdiff(unique(spread$Cohort),"Control")
k <- 1
for(cohort in target.cohorts){
  df.sub <- spread[which(spread$Cohort%in%c(cohort,"Control")),]
  df.sub$stack <- k
  spread_stacked <- rbind(spread_stacked,df.sub)
  k <- k + 1
}
spread_stacked$st_unit <- as.numeric(factor(paste0(spread_stacked$stack,'-',spread_stacked$fips)))
spread_stacked$st_year <- as.numeric(factor(paste0(spread_stacked$stack,'-',spread_stacked$year)))

spread$event_year = ifelse( spread$event_year >= 6 , 6, spread$event_year  )
spread$event_year = ifelse( spread$event_year <= -6 , -6, spread$event_year  )

plot_stacked <- feols(statetaxSPREAD_MMAinterp ~  i(event_year, 0)
                      + lnsize
                      + lnmat
                      + BQ
                      + GO
                      + log_pop
                      + callable
                      + insured_mergent
                      + cum_outbreak_count
                      + q1mintemp + q2mintemp + q3mintemp + q4mintemp
                      + q1precip + q2precip + q3precip +  q4precip
                      | st_unit + st_year + agg_rating_min_lt , cluster ~ fips + year,
                      fixef.rm =  "singleton",
                      data = spread_stacked
)
summary(plot_stacked)

coefficients <- coef(plot_stacked)
se <- se(plot_stacked)
conf_intervals <- confint(plot_stacked, level = 0.90)
result_stacked <- data.frame(
  Estimate = coefficients,
  SE = se,
  Lower_CI = conf_intervals[, 1],
  Upper_CI = conf_intervals[, 2]
)
result_stacked = result_stacked[1:12,]
zero_df = data.frame( Estimate = 0, SE = 0, Lower_CI = 0, Upper_CI = 0)
result_stacked = rbind(zero_df, result_stacked)
result_stacked$term = c(0, -6:-1, 1:6)
result_stacked$Model = "Cengiz et. al (2019)"

#----------------------------------------------------------------
# Borusyak et al. (2024) imputation estimator
#----------------------------------------------------------------

model.impute <- did_imputation(data = spread,
                               yname = "statetaxSPREAD_MMAinterp",
                               gname = "treatment_year",
                               tname = "year",
                               idname = "fips",
                               cluster_var = "fips",
                               pretrends = c(-6:-1),
                               horizon = TRUE,
                               first_stage = ~ lnsize
                               + lnmat
                               + BQ
                               + GO
                               + callable
                               + insured_mergent
                               | fips + year + agg_rating_min_lt)
model.impute$term <- as.numeric(model.impute$term)
result_borusyak <- as.data.frame(model.impute)

result_borusyak$Model = "Borusyak et. al (2024)"
result_borusyak$lhs = NULL
result_borusyak = result_borusyak %>% filter(term > -7 & term < 7)
names(result_borusyak) = c("term", "Estimate", "SE", "Lower_CI", "Upper_CI", "Model")

#----------------------------------------------------------------
# Combine results and create figure
#----------------------------------------------------------------

combined_data = rbind(result_df, result_df_no_controls, result_stacked, result_borusyak)

dodge_width <- 0.7

showtext_auto()

pdf(file = '../../tables_figures_SI/figure_SI-2_panel_a.pdf', width = 18)

ggplot(combined_data, aes(x = term, y = Estimate, color = Model, shape = Model)) +
  geom_point(size = 6, position = position_dodge(width = dodge_width), aes(group = Model)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, group = Model),
                width = 0.5,
                linewidth = 1,
                position = position_dodge(width = dodge_width))+
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    y = "Bond Spread", x = "Event Year",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(
    values = c(
      "With full controls" = alpha("black"),
      "No controls" = alpha("#5BBCD6", 0.6),
      "Cengiz et. al (2019)" = alpha("#F2AD00", 0.6),
      "Borusyak et. al (2024)" = alpha("#00A08A", 0.6)
    )
  ) +
  scale_shape_manual(
    values = c("With full controls" = 15, "No controls" = 16,
               "Cengiz et. al (2019)" = 17,"Borusyak et. al (2024)" = 18
    ) ) +
  scale_x_continuous(
    breaks = seq(-6, 6, 1),
    labels = seq(-6, 6, 1),
  ) +
  geom_hline(yintercept = 11.54, linetype = "dashed", color = "black", size = 2) +
  scale_y_continuous(
    limits = c(-30, 50),
    breaks = c(-30, -20, -10, 0, 11.54, 20, 30, 40, 50),
    labels = c("-30", "-20", "-10", "0", "11.54", "20", "30", "40", "50")
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Helvetica"),
    axis.title.x = element_text(size = 35, color = "black", margin = margin(t = 10)),
    axis.title.y = element_text(size = 35, color = "black"),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    legend.text = element_text(size = 30, color = "black")
  )
dev.off()
