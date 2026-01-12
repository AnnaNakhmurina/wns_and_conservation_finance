#----------------------------------------------------------------
# Figure SI-1, Panel A: Maps of county sample coverage.
#
# This script creates two maps showing the geographic distribution of
# counties by WNS treatment status and data availability: one for
# financial data coverage, one for bond data coverage.
#----------------------------------------------------------------

rm(list = ls())
gc()

#----------------------------------------------------------------
# Load packages
#----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(haven)
library(showtext)
library(tigris)
library(sf)

# Load and use a modern font
font_add_google("Lato", "lato")
showtext_auto()

#----------------------------------------------------------------
# Load data
#----------------------------------------------------------------

# Load county level data and subset it to rural counties
data <- read_dta("../../../data/2024/bats_county_financial_data.dta")

treat <- data %>% dplyr::select(fips, rural_county, release_year) %>% unique()
treat$type <- ifelse(treat$rural_county == 1, "Rural", "Urban")
treat$treated <- ifelse(!is.na(treat$release_year) & treat$release_year <= 2022, "WNS", "no WNS")

#----------------------------------------------------------------
# Prepare county shapefile
#----------------------------------------------------------------

# Use tigris to get the US counties shapefile
counties <- counties(cb = TRUE, resolution = "20m")

# Convert to sf object
counties_sf <- st_as_sf(counties)

# Filter for continental US (excluding Alaska, Hawaii, and territories)
continental_states <- c("01", "04", "05", "06", "08", "09", "10", "11", "12", "13",
                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
                        "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
                        "36", "37", "38", "39", "40", "41", "42", "44", "45", "46",
                        "47", "48", "49", "50", "51", "53", "54", "55", "56")

counties_continental <- counties_sf %>%
  filter(STATEFP %in% continental_states)
counties_continental$GEOID <- as.numeric(counties_continental$GEOID)

counties_continental <- left_join(counties_continental, treat, by = c("GEOID" = "fips"))
counties_continental$type <- ifelse(is.na(counties_continental$type), "Missing Fin. Data", counties_continental$type)
counties_continental$treated <- ifelse(is.na(counties_continental$treated), "", counties_continental$treated)

counties_continental$type_wns <- paste0(counties_continental$type, " ", counties_continental$treated)

#----------------------------------------------------------------
# Map 1: Financial data coverage
#----------------------------------------------------------------

# Compute the number of counties in each group
rural_no_wns_fin <- counties_continental %>% filter(type_wns == "Rural no WNS") %>% nrow()
rural_wns_fin <- counties_continental %>% filter(type_wns == "Rural WNS") %>% nrow()
urban_no_wns_fin <- counties_continental %>% filter(type_wns == "Urban no WNS") %>% nrow()
urban_wns_fin <- counties_continental %>% filter(type_wns == "Urban WNS") %>% nrow()
no_data_fin <- counties_continental %>% filter(type_wns == "Missing Fin. Data ") %>% nrow()

# Plot the map using ggplot2, colored by the categorical variable
map <- ggplot(data = counties_continental) +
  geom_sf(aes(fill = type_wns), color = NA) +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50)) +
  scale_fill_manual(
    values = c("Rural WNS" = "#4dac26", "Urban WNS" = "#d01c8b",
               "Rural no WNS" = "#b8e186", "Urban no WNS" = "#f1b6da",
               "Missing Fin. Data " = "white"),
    labels = c(paste0("Missing Fin. Data", " (", no_data_fin, ")"),
               paste0("Rural no WNS", " (", rural_no_wns_fin, ")"),
               paste0("Rural WNS", " (", rural_wns_fin, ")"),
               paste0("Urban no WNS", " (", urban_no_wns_fin, ")"),
               paste0("Urban WNS", " (", urban_wns_fin, ")"))  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20),
    panel.border = element_blank()
  ) +
  labs(fill = NULL) +
  guides(fill = guide_legend(nrow = 2, title = NULL))

map

ggsave(paste0('../../tables_figures_SI/', 'figure_SI-1_panel_b.pdf'), map,
       width = 12, height = 10, dpi = 300, units = "in")

#----------------------------------------------------------------
# Map 2: Bond data coverage
#----------------------------------------------------------------

counties_continental$STATEFP <- as.numeric(counties_continental$STATEFP)
counties_continental$COUNTYFP <- as.numeric(counties_continental$COUNTYFP)
counties_continental$identif <- paste0(counties_continental$STATEFP, "_", counties_continental$COUNTYFP)

spread <- read_dta(paste0("../../../data/2024/bats_county_issuers_data.dta"))
spread <- spread %>% filter(!is.na(statetaxSPREAD_MMAinterp) & !is.na(q3mintemp))

'%!in%' <- function(x, y) !('%in%'(x, y))

counties_continental$type_wns_bond <- ifelse(counties_continental$identif %!in% spread$identif, "Missing Bond Data no WNS", counties_continental$type_wns)
counties_continental$type_wns_bond <- ifelse(counties_continental$type_wns_bond == "Missing Bond Data no WNS" & counties_continental$treated == "WNS", "Missing Bond Data WNS", counties_continental$type_wns_bond)

# Compute the number of counties in each bond group
rural_no_wns_bond <- counties_continental %>% filter(type_wns_bond == "Rural no WNS") %>% nrow()
rural_wns_bond <- counties_continental %>% filter(type_wns_bond == "Rural WNS") %>% nrow()
urban_no_wns_bond <- counties_continental %>% filter(type_wns_bond == "Urban no WNS") %>% nrow()
urban_wns_bond <- counties_continental %>% filter(type_wns_bond == "Urban WNS") %>% nrow()
no_data_wns_bond <- counties_continental %>% filter(type_wns_bond == "Missing Bond Data WNS") %>% nrow()
no_data_no_wns_bond <- counties_continental %>% filter(type_wns_bond == "Missing Bond Data no WNS") %>% nrow()

# Colors from here: https://colorbrewer2.org/#type=diverging&scheme=PiYG&n=4
map <- ggplot(data = counties_continental) +
  geom_sf(aes(fill = type_wns_bond), color = NA) +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50)) +
  scale_fill_manual(
    values = c("Rural WNS" = "#4dac26", "Urban WNS" = "#d01c8b",
               "Rural no WNS" = "#b8e186", "Urban no WNS" = "#f1b6da",
               "Missing Bond Data no WNS" = "white", "Missing Bond Data WNS" = "black"),
    labels = c(paste0("Missing Bond Data no WNS", " (", no_data_no_wns_bond, ")"),
               paste0("Missing Bond Data WNS", " (", no_data_wns_bond, ")"),
               paste0("Rural no WNS", " (", rural_no_wns_bond, ")"),
               paste0("Rural WNS", " (", rural_wns_bond, ")"),
               paste0("Urban no WNS", " (", urban_no_wns_bond, ")"),
               paste0("Urban WNS", " (", urban_wns_bond, ")"))
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20),
    panel.border = element_blank()
  ) +
  labs(fill = "")

map

ggsave(paste0('../../tables_figures_SI/', 'figure_SI-1_panel_a.pdf'), map,
       width = 12, height = 10, dpi = 300, units = "in")
