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
#source(paste(pwcdir, "src/02_write_update_vvwm.R",sep =""))


# QAQC check ---------------------------------------------------------------   ????????
#source(paste(pwcdir, "src/03b_przm_qaqc.R",sep=""))


# read, write all outputs to .Rdata ----------------------------------------
source(paste(pwcdir,"src/05_write_ouput_into_df.R",sep = ""))


# run sensitivity analysis w/ daily conc. ----------------------------------
source(paste(pwcdir,"src/06a_sensitivity_analysis_linear_daily.R",sep = ""))


# run sensitivity analysis w/ daily max conc. ------------------------------
source(paste(pwcdir,"src/06b_sensitivity_analysis_linear_max.R",sep = ""))


# sensitivity graphics -----------------------------------------------------
source(paste(pwcdir,"src/07sensitivity_analyses_graphics.R",sep = ""))


# maximum sensitivity graphics ---------------------------------------------   ????????
#source(paste(pwcdir, "src/07a_max_sensitivity_analysis_graphics.R", sep="") 


# sensitivity parameter distribution graphs --------------------------------
source(paste(pwcdir,"src/08pardistribution.R",sep = ""))


# plot some outputs --------------------------------------------------------  ?????????
#source(paste(pwcdir, "src/plotting_output.R", sep=""))


# plot stream concentrations -----------------------------------------------  ????????
#source(paste(pwcdir, "src/plotting_with_observeddata.R",sep=""))


# process PUR report -------------------------------------------------------  ????????
#source(paste(pwcdir, "src/pur_section_pesticide.R",sep=""))




# --------------------------------------------------------------------------
# the end
# --------------------------------------------------------------------------