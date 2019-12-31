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

# ------------------------------------------------------------------------------
# Run sensitivity analysis with daily concentrations
# ------------------------------------------------------------------------------


  
ndays <- length(timearray)

## load przm output

dim(pwcoutdf)
pwcoutdf[1:10,,1]#check output

pwch2output <- pwcoutdf[,2,1:Nsims]#1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
plot(pwcoutdf[,2,1:Nsims])
dim(pwch2output)# simulations x variables
dim(inputdata)# days x simulations
nvars <- length(inputdata)#number of input variables

# create partial correlation coefficients array for input
tarray_pwcpccout<- array(data=NA, c(ndays,nvars))

# daily partial correlation coefficients
for (i in 1:ndays){  #break
  out_sim<- pwch2output[i,1:Nsims]
  
  temp_pcc<- pcc(inputdata[1:Nsims,], out_sim, rank = F)
  print(paste(i,"out of",ndays)) 
  tarray_pwcpccout[i,] <- temp_pcc$PCC[[1]]
}

# write control pcc results to disk

dim(tarray_pwcpccout)
save(tarray_pwcpccout,file = paste(pwcdir,"io/tarray_pwcpccout.RData", sep = ""))
write.csv(tarray_pwcpccout, file = paste(pwcdir, "io/tarray_pccout.csv", sep = ""))

plot(temp_pcc)


