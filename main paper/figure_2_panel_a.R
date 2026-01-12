#----------------------------------------------------------------
# Figure 2, Panel A: Event-study plot of property tax revenue.
#
# This script estimates an event-study regression of property tax
# revenue per capita on WNS outbreak timing, then creates an event study plot
#----------------------------------------------------------------

rm(list = ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(fixest)
library(ggplot2)
library(ggfixest)
library(haven)

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

data <- read_dta("../../../data/2024/bats_rural_financial_data.dta")

#----------------------------------------------------------------
# Create an event-time plot with 90% and 85% confidence intervals
#----------------------------------------------------------------

plot <- feols(
  property_tax_per_capita ~ i(event_year, 0)
  + lnpop
  + cum_outbreak_count
  + q1mintemp + q2mintemp + q3mintemp + q4mintemp
  + q1precip + q2precip + q3precip + q4precip
  | year + fips,
  cluster ~ fips,
  fixef.rm = "singleton",
  data = data
)
summary(plot)

# Build the three event-study plots at different CI levels
p95 <- ggiplot(plot, ref.line = FALSE, geom_style = "errorbar",
               xlab = "Event Year", main = "", grid = TRUE, pt.cex = 3,
               ylab = "Property tax revenue per capita", ci_level = 0.95)

p90 <- ggiplot(plot, ref.line = FALSE, geom_style = "errorbar",
               xlab = "Event Year", main = "", grid = TRUE, pt.cex = 3,
               ylab = "Property tax revenue per capita", ci_level = 0.90)

p85_tmp <- ggiplot(plot, ref.line = FALSE, geom_style = "errorbar",
                   xlab = "Event Year", main = "", grid = TRUE, pt.cex = 3,
                   ylab = "Property tax revenue per capita", ci_level = 0.85)

# Extract CI data from each build
pb95 <- ggplot_build(p95)
pb90 <- ggplot_build(p90)
pb85 <- ggplot_build(p85_tmp)

get_ci_idx <- function(p) {
  which(sapply(p$layers, function(l)
    inherits(l$geom, "GeomErrorbar") || inherits(l$geom, "GeomRibbon")))[1]
}

ci_idx_95 <- get_ci_idx(p95)
ci_idx_90 <- get_ci_idx(p90)
ci_idx_85 <- get_ci_idx(p85_tmp)

# NOTE: fix bug: take 95% data from pb95 (not pb90)
ci95_dat <- pb95$data[[ci_idx_95]]
ci90_dat <- pb90$data[[ci_idx_90]]
ci85_dat <- pb85$data[[ci_idx_85]]

ci95_dat$CI <- "95% CI"
ci90_dat$CI <- "90% CI"
ci85_dat$CI <- "85% CI"

ci_all <- rbind(
  ci95_dat[, c("x", "ymin", "ymax", "CI")],
  ci90_dat[, c("x", "ymin", "ymax", "CI")],
  ci85_dat[, c("x", "ymin", "ymax", "CI")]
)

# Remove original CI layer from the base (90%) plot to avoid double-plotting
if (!is.na(ci_idx_90)) p90$layers[ci_idx_90] <- NULL

# Compose the final figure
bar <- p90 +
  geom_errorbar(
    data = ci_all,
    inherit.aes = FALSE,
    aes(x = x, ymin = ymin, ymax = ymax, color = CI),
    width = 0.2,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c("95% CI" = "grey80",
               "90% CI" = "grey55",
               "85% CI" = "black"),
    breaks = c("95% CI", "90% CI", "85% CI")
  ) +
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 1.2))) +
  scale_x_continuous(
    breaks = seq(-6, 6, 1),
    labels = c("\u2264-6", seq(-5, 5, 1), "\u22656")
  ) +
  geom_point(size = 5, show.legend = FALSE) +
  geom_hline(yintercept = -75.63, linetype = "dashed", color = "black", size = 2) +
  scale_y_continuous(
    limits = c(-150, 100),
    breaks = c(-150, -100, -75.63, -50, 0, 50, 100),
    labels = c("-150", "-100", "-75.64", "-50", "0", "50", "100")
  ) +
  theme(
    text = element_text(family = "Helvetica"),
    axis.title.x = element_text(size = 50),
    axis.title.y = element_text(size = 50),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 45),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
bar

ggsave(paste0('../../tables_figures_main/', 'figure2_panel_a.pdf'), bar,
       width = 32, height = 10, dpi = 300, units = "in")
