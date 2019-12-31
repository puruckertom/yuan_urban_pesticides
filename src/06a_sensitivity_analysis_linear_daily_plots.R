# ------------------------------------------------------------------------------
# Run sensitivity analysis with daily concentrations
# ------------------------------------------------------------------------------



# -------------------------------------------------------------------------
# load output files
# -------------------------------------------------------------------------

# przm output array
# recall: this was created in 03write_update_run_pwc and saved in 05_write_output_into_df
# recall: this is an array of all of the output.zts files  (dim = num_of_days*output_cols*sims)
load(paste(pwcdir, "io/przmout.RData", sep = ""))
dim(outputdf)

# pwc output array
# recall: this was created in 03write_update_run_pwc and saved in 05_write_output_into_df
# recall: this is an array of all of the output_11_parent_only_Custom_Parent_daily.csv files 
load(paste(pwcdir, "io/pwcout.RData", sep = ""))
dim(pwcoutdf)

# conversion factor dataframe  
# recall: this was created in 03write_update_run_pwc
# recall: this is an array of all of the output_11_parent_only_Custom_Parent.txt files
load(paste(pwcdir, "io/con_fac_output.RData", sep = ""))
dim(con_fac_output)
names(con_fac_output)


# time 
# recall: this was created in 05_write_output_into_df
load(paste(pwcdir,"io/timearray.RData", sep = ""))



# -------------------------------------------------------------------------------
# Percentiles (RUNF0)
# -------------------------------------------------------------------------------


# subset przm H2O 
przm_h2_output <- (outputdf[,4,1:Nsims]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
head(przm_h2_output)
dim(przm_h2_output) #days*simulations


# still more to write......



# -------------------------------------------------------------------------------
# PCC (max RUNF0)
# -------------------------------------------------------------------------------

# subset max przm H2O
przm_max_h2 <-apply(przm_h2_output, 2, function(x) max(x, na.rm = TRUE))

# still more to write......




# -------------------------------------------------------------------------------
# PCC (Ave.Conc.H2O)
# -------------------------------------------------------------------------------

ndays <- length(timearray)

# check pwc output
dim(pwcoutdf)
pwcoutdf[1:10,,1]
pwcoutdf[1890:1900,,1]
pwcoutdf[2034:2044,,1]

# subset Ave.Conc.H2O (model output) for PCC
pwc_h2_output <- pwcoutdf[,2,1:Nsims] #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
dim(pwc_h2_output) #days*simulations

plot(pwc_h2_output) 
plot(pwc_h2_output[,1], type = "l") #time series for simulation #1
plot(pwc_h2_output[,5], type = "l") #time series for simulation #5


# check LHS 
dim(inputs_lhs) #simulations*variables
nvars <- length(inputs_lhs) #number of input variables


# create BLANK partial correlation coefficients array for input
tarray_pcc_ave_conc_daily<- array(data=NA, c(ndays,nvars))
dim(tarray_pcc_ave_conc_daily) #days*variables



# partial correlation coefficients
for (i in 1:ndays){  
  out_sim<- pwc_h2_output[i,1:Nsims]
  in_sims <- inputs_lhs[1:Nsims,]
  temp_pcc<- pcc(in_sims, out_sim, rank = F)
  
  print(paste(i,"out of",ndays)) 
  tarray_pcc_ave_conc_daily[i,] <- temp_pcc$PCC[[1]]
}

# write out pcc results
dim(tarray_pcc_ave_conc_daily)

save(tarray_pcc_ave_conc_daily,file = paste(pwcdir,"io/tarray_pcc_ave_conc_daily.RData", sep = ""))
write.csv(tarray_pcc_ave_conc_daily, file = paste(pwcdir, "io/tarray_pcc_ave_conc_daily.csv", sep = ""))

# plot
plot(temp_pcc)

ggplot(temp_pcc) +
  geom_bar(stat = "identity",color = "orange", fill = "orange") 



# -------------------------------------------------------------------------------
# PCC (Peak.Conc.H20)
# -------------------------------------------------------------------------------

ndays <- length(timearray)

# check pwc output
dim(pwcoutdf)
pwcoutdf[1:10,,1]
pwcoutdf[1890:1900,,1]
pwcoutdf[2034:2044,,1]

# subset Peak.Conc.H2O (model output) for PCC
pwc_peak_output <- pwcoutdf[,4,1:Nsims] #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
dim(pwc_peak_output) #days*simulations

plot(pwc_peak_output) 
plot(pwc_peak_output[,1], type = "l") #time series for simulation #1
plot(pwc_peak_output[,5], type = "l") #time series for simulation #5


# check LHS 
dim(inputs_lhs) #simulations*variables
nvars <- length(inputs_lhs) #number of input variables


# create BLANK partial correlation coefficients array for input
tarray_pcc_peak_daily<- array(data=NA, c(ndays,nvars))
dim(tarray_pcc_peak_daily) #days*variables



# partial correlation coefficients
for (i in 1:ndays){  
  out_sim<- pwc_peak_output[i,1:Nsims]
  in_sims <- inputs_lhs[1:Nsims,]
  temp_pcc_peak<- pcc(in_sims, out_sim, rank = F)
  
  print(paste(i,"out of",ndays)) 
  tarray_pcc_peak_daily[i,] <- temp_pcc_peak$PCC[[1]]
}

# write out pcc results
dim(tarray_pcc_peak_daily)

save(tarray_pcc_peak_daily,file = paste(pwcdir,"io/tarray_pcc_peak_daily.RData", sep = ""))
write.csv(tarray_pcc_peak_daily, file = paste(pwcdir, "io/tarray_pcc_peak_daily.csv", sep = ""))

# plot
plot(temp_pcc_peak)

ggplot(temp_pcc_peak) +
  geom_bar(stat = "identity",color = "orange", fill = "orange") 




# -------------------------------------------------------------------------------
# PCC (Ave.Conc.benth)
# -------------------------------------------------------------------------------

ndays <- length(timearray)

# check pwc output
dim(pwcoutdf)
pwcoutdf[1:10,,1]
pwcoutdf[1890:1900,,1]
pwcoutdf[2034:2044,,1]

# subset Peak.Conc.H2O (model output) for PCC
pwc_ben_output <- pwcoutdf[,3,1:Nsims] #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
dim(pwc_ben_output) #days*simulations

plot(pwc_ben_output) 
plot(pwc_ben_output[,1], type = "l") #time series for simulation #1
plot(pwc_ben_output[,5], type = "l") #time series for simulation #5


# check LHS 
dim(inputs_lhs) #simulations*variables
nvars <- length(inputs_lhs) #number of input variables


# create BLANK partial correlation coefficients array for input
tarray_pcc_benthic_daily<- array(data=NA, c(ndays,nvars))
dim(tarray_pcc_benthic_daily) #days*variables



# partial correlation coefficients
for (i in 1:ndays){  
  out_sim<- pwc_ben_output[i,1:Nsims]
  in_sims <- inputs_lhs[1:Nsims,]
  temp_pcc_benthic<- pcc(in_sims, out_sim, rank = F)
  
  print(paste(i,"out of",ndays)) 
  tarray_pcc_benthic_daily[i,] <- temp_pcc_benthic$PCC[[1]]
}

# write out pcc results
dim(tarray_pcc_benthic_daily)

save(tarray_pcc_benthic_daily,file = paste(pwcdir,"io/tarray_pcc_benthic_daily.RData", sep = ""))
write.csv(tarray_pcc_benthic_daily, file = paste(pwcdir, "io/tarray_pcc_benthic_daily.csv", sep = ""))

# plot
plot(temp_pcc_benthic)

ggplot(temp_pcc_benthic) +
  geom_bar(stat = "identity",color = "orange", fill = "orange") 



# -------------------------------------------------------------------------------
# PCC (max Ave.Conc.benth)
# -------------------------------------------------------------------------------

# calculate MAX
pwc_max_benthic <-apply(pwc_ben_output, 2, function(x) max(x, na.rm = TRUE))

# -------------------------------------------------------------------------------
# the end
# -------------------------------------------------------------------------------