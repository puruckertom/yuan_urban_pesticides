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
# PCC (btw Ave.Conc.H2O and Input Variables (from LHS))
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
tarray_pwc_pcc_out_daily<- array(data=NA, c(ndays,nvars))
dim(tarray_pwc_pcc_out_daily) #days*variables



# partial correlation coefficients
for (i in 1:ndays){  #break
  out_sim<- pwc_h2_output[i,1:Nsims]
  in_sims <- inputs_lhs[1:Nsims,]
  temp_pcc<- pcc(in_sims, out_sim, rank = F)
  
  print(paste(i,"out of",ndays)) 
  tarray_pwc_pcc_out_daily[i,] <- temp_pcc$PCC[[1]]
}

# write out pcc results
dim(tarray_pwc_pcc_out_daily)

save(tarray_pwc_pcc_out_daily,file = paste(pwcdir,"io/tarray_pwc_pcc_out_daily.RData", sep = ""))
write.csv(tarray_pwc_pcc_out_daily, file = paste(pwcdir, "io/tarray_pwc_pcc_out_daily.csv", sep = ""))

# plot
plot(temp_pcc)



# # ----------------------------------------------------------------------------
# # Control
# # ----------------------------------------------------------------------------
# ndays <- length(timearray)
# 
# ## load przm output
# 
# dim(outputdf)
# outputdf[1:10,,1]#check output
# 
# pestoutput <- outputdf[,7,1:Nsims]#6pesticide concentration in runoff, 7 pesticide concentration in erosion,8pesticide concentration in pore water
# 
# dim(pestoutput)
# dim(inputdata)
# nvars <- length(inputdata)#number of input variables
# dim(nvars)
# #create partial correlation coefficients array for output
# tarray_pccout<- array(data=NA, c(ndays,nvars))#create time series input array
# 
# #partial correlation coefficients
# for (i in 1:ndays){  #break
#   temp<- pestoutput[i,1:Nsims]
#   #inputdata$PFAC <- tdarray[i,27,] 
#   temp_pcc<- pcc(inputdata, temp, rank = F)
#   print(paste(i,"out of",ndays)) 
#   tarray_pccout[i,] <- temp_pcc$PCC[[1]]
# }
# 
# #write control pcc results to disk
# dim(tarray_pccout)
# save(tarray_pccout,file = paste(pwcdir,"io/tarray_pccout.RData", sep = ""))
# #write.csv(tarray_pccout, file = paste(pwcdir, "io/tarray_pccout.csv", sep = ""))


# --------------------------------------------------------------------------------------
# the end
# --------------------------------------------------------------------------------------