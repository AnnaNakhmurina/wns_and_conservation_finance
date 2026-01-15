# ==============================================================================
# MASTER SCRIPT: "The fiscal impact of biodiversity loss and a pathway for conservation finance" 
# (forthcoming at Science) by Nakhmurina, Manning, and Fenichel. 
# ==============================================================================
# This master script runs all analyses for main paper and 
# supplementary materials the paper. 
#
# Note: Scripts must be run in order
# ==============================================================================

cat("=================================================================\n")
cat("Starting master script execution\n")
cat("=================================================================\n\n")

#----------------------------------------------------------------
# Main paper figures
#----------------------------------------------------------------

cat("Running main paper codes...\n")
cat("-----------------------------------------------------------------\n")

source("main paper/figure_1_panel_a.R")
cat("Completed: figure_1_panel_a.R\n")

source("main paper/figure_1_panel_b.R")
cat("Completed: figure_1_panel_b.R\n")

source("main paper/figure_2_panel_a.R")
cat("Completed: figure_2_panel_a.R\n")

source("main paper/figure_2_panel_b.R")
cat("Completed: figure_2_panel_b.R\n")

cat("\nMain paper codes complete.\n\n")

#----------------------------------------------------------------
# Supplementary analyses
#----------------------------------------------------------------

cat("Running supplementary analyses...\n")
cat("-----------------------------------------------------------------\n")

# Figure SI-1
source("supplemental material/1. figure_SI-1.R")
cat("Completed: 1. figure_SI-1.R\n")

# Figure SI-2
source("supplemental material/2. figure_SI-2_panel_a.R")
cat("Completed: 2. figure_SI-2_panel_a.R\n")

source("supplemental material/2. figure_SI-2_panel_b.R")
cat("Completed: 2. figure_SI-2_panel_b.R\n")

source("supplemental material/2. figure_SI-2_panel_c.R")
cat("Completed: 2. figure_SI-2_panel_c.R\n")

# Figure SI-3
source("supplemental material/3. figure_SI-3.R")
cat("Completed: 3. figure_SI-3.R\n")

# Table SI-1
source("supplemental material/4. table SI-1.R")
cat("Completed: 4. table SI-1.R\n")

# Table SI-2
source("supplemental material/5. table SI-2.R")
cat("Completed: 5. table SI-2.R\n")

# Table SI-3
source("supplemental material/6. table SI-3_panel_a.R")
cat("Completed: 6. table SI-3_panel_a.R\n")

source("supplemental material/6. table SI-3_panel_b.R")
cat("Completed: 6. table SI-3_panel_b.R\n")

source("supplemental material/6. table SI-3_panel_c.R")
cat("Completed: 6. table SI-3_panel_c.R\n")

# Table SI-4
source("supplemental material/7. table SI-4_panel_a.R")
cat("Completed: 7. table SI-4_panel_a.R\n")

source("supplemental material/7. table SI-4_panel_b.R")
cat("Completed: 7. table SI-4_panel_b.R\n")

# Table SI-5
source("supplemental material/8. table SI-5_panel_a.R")
cat("Completed: 8. table SI-5_panel_a.R\n")

source("supplemental material/8. table SI-5_panel_b.R")
cat("Completed: 8. table SI-5_panel_b.R\n")

# Table SI-6
source("supplemental material/9. table SI-6_panel_a.R")
cat("Completed: 9. table SI-6_panel_a.R\n")

source("supplemental material/9. table SI-6_panel_b.R")
cat("Completed: 9. table SI-6_panel_b.R\n")

# Table SI-7
source("supplemental material/10. table SI-7_panel_a.R")
cat("Completed: 10. table SI-7_panel_a.R\n")

source("supplemental material/10. table SI-7_panel_b.R")
cat("Completed: 10. table SI-7_panel_b.R\n")

# Table SI-8
source("supplemental material/11. table SI-8_panel_a.R")
cat("Completed: 11. table SI-8_panel_a.R\n")

source("supplemental material/11. table SI-8_panel_b.R")
cat("Completed: 11. table SI-8_panel_b.R\n")

# Table SI-9
source("supplemental material/12. table SI-9_panel_a.R")
cat("Completed: 12. table SI-9_panel_a.R\n")

source("supplemental material/12. table SI-9_panel_b.R")
cat("Completed: 12. table SI-9_panel_b.R\n")

# Table SI-10
source("supplemental material/13. table SI-10.R")
cat("Completed: 13. table SI-10.R\n")

# Table SI-11
source("supplemental material/14. table SI-11.R")
cat("Completed: 14. table SI-11.R\n")

# Table SI-12
source("supplemental material/15. table SI-12.R")
cat("Completed: 15. table SI-12.R\n")

# Table SI-13
source("supplemental material/16. table SI-13.R")
cat("Completed: 16. table SI-13.R\n")

# Table SI-14
source("supplemental material/17. table SI-14_panel_a.R")
cat("Completed: 17. table SI-14_panel_a.R\n")

source("supplemental material/17. table SI-14_panel_b.R")
cat("Completed: 17. table SI-14_panel_b.R\n")

# Table SI-15
source("supplemental material/18. table SI-15.R")
cat("Completed: 18. table SI-15.R\n")

# Table SI-16
source("supplemental material/19. table SI-16_panel_a.R")
cat("Completed: 19. table SI-16_panel_a.R\n")

source("supplemental material/19. table SI-16_panel_b.R")
cat("Completed: 19. table SI-16_panel_b.R\n")

# Table SI-17
source("supplemental material/20. table SI-17_panel_a.R")
cat("Completed: 20. table SI-17_panel_a.R\n")

source("supplemental material/20. table SI-17_panel_b.R")
cat("Completed: 20. table SI-17_panel_b.R\n")

source("supplemental material/20. table SI-17_panel_c.R")
cat("Completed: 20. table SI-17_panel_c.R\n")

# Table SI-18
source("supplemental material/21. table SI-18.R")
cat("Completed: 21. table SI-18.R\n")

cat("\nSupplementary analyses complete.\n")

cat("\n=================================================================\n")
cat("Master script execution finished\n")
cat("=================================================================\n")
