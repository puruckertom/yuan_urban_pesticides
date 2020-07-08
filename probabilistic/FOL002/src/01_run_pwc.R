# --------------------------------------------------------------------------
# run everything- only once everything is ready to go!
# --------------------------------------------------------------------------

#  parameterize inputs -----------------------------------------------------
#source(paste(pwcdir,"src/01a_parameterization.R",sep = ""))
source(paste(pwcdir,"src/02_lhs_parameterization.R",sep = ""))


# conduct Nsims of PWC -----------------------------------------------------
source(paste(pwcdir,"src/03_write_update_run_pwc.R",sep = ""))


# run VVWM and produce VVWMtransfer.txt ------------------------------------   
# this file gets run in 03_write_update_run_pwc.R
#source(paste(pwcdir, "src/04_write_update_vvwm.R",sep =""))


# read, write all outputs to .Rdata ----------------------------------------
source(paste(pwcdir,"src/05_write_ouput_into_df.R",sep = ""))


# compute pcc w/ daily conc. ----------------------------------
#source(paste(pwcdir,"src/06a_pcc_computation_daily.R",sep = ""))


# compute pcc w/ daily max conc. ------------------------------
source(paste(pwcdir,"src/06b_pcc_computation_max.R",sep = ""))


# sensitivity graphics for daily--------------------------------------------
#source(paste(pwcdir,"src/07a_pcc_graphics_daily.R",sep = ""))
#source(paste(pwcdir,"src/07a2_pcc_graphics_daily.R",sep = ""))


# sensitivity graphics for max ---------------------------------------------   
source(paste(pwcdir, "src/07b_pcc_graphics_max.R", sep="")) 


# sensitivity parameter distribution graphs --------------------------------
source(paste(pwcdir, "src/08_percentile_graphics_with_observed_data_and_alb_panels.R", sep="")) 


# --------------------------------------------------------------------------
# the end
# --------------------------------------------------------------------------
