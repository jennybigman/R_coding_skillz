# how to make a table from model output and export it as PDF, PNG, etc.	

######## NEED TO SIMULATE DATA INSTEAD OF USING POLLOCK_DAT BELOW ###########
	library(gt)
  library(gtsummary)
	library(modelsummary)
	library(here)

	pollock_dat <- read.csv(here("lagdat.csv"))
  
  mod1 <- lm(log_sc_weight ~ julian, data = pollock_dat)
  mod2 <- lm(log_sc_weight ~ sst.amj + julian, data = pollock_dat)
  mod3 <- lm(log_sc_weight ~ sst.amj * as.factor(AGE) + julian, data = pollock_dat)

  mod3_sum <- summary(mod3)[[4]] %>% as.data.frame()
  
  mod3_int <- mod3_sum[1,1]
  mod3_ints <- mod3_int + mod3_sum[3:16, 1]
  mod3_ints <- c(mod3_int, mod3_ints)
  
  mod3_slope <- mod3_sum[2,1]
  mod3_slopes <- mod3_slope + mod3_sum[18:31, 1]
  mod3_slopes <- c(mod3_slope, mod3_slopes)
 
  age <- 1:15

  mod3_table <- cbind(age, mod3_ints, mod3_slopes) %>%
  	as.data.frame() %>%
  	rename(intercept = mod3_ints,
  				 slope = mod3_slopes) %>%
  	round(2)
  	
 mod3_table_gt <- gt(mod3_table)
 
 gtsave(mod3_table_gt, file = here("./example_table.docx"))
	