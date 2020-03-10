# -----------------------------------------------------------------------------
# read, write all outputs to .Rdata
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# create time dataframe
# -----------------------------------------------------------------------------

# create a sequence of the time dates from start to end
timearray <-seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")#format 1961-01-01

# change the date format
timearray <- as.Date(timearray,"%m/%d/%Y")

timediff <- timearray[3]-timearray[2]
timearray[1] <- timearray[2]-timediff
length(timearray)

# write out
save(timearray, file = paste(pwcdir,"io/timearray.RData",sep = ""))
write.csv(timearray, file = paste(pwcdir, "io/timearray.csv", sep = ""))




# -----------------------------------------------------------------------------
# read przm output array and save as .Rdata
# -----------------------------------------------------------------------------

# recall: outputdf created in 03write_update_run_pwc.R
dim(outputdf) # dim = num_of_days*output_cols*sims

# update colnames
colnames(outputdf) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")

# write out
save(outputdf, file = paste(pwcdir,"io/przmout.RData",sep = ""))
write.csv(outputdf, file = paste(pwcdir, "io/przmout.csv", sep = ""))



# -----------------------------------------------------------------------------
# read pwc output and save as .Rdata
# -----------------------------------------------------------------------------
                  
# recall: pwcoutdf created in 03write_update_run_pwc.R
dim(pwcoutdf)

# update colnames
colnames(pwcoutdf) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# write out
save(pwcoutdf, file = paste(pwcdir,"io/pwcout.RData", sep = ""))
write.csv(pwcoutdf, file = paste(pwcdir, "io/pwcout.csv", sep = ""))




# -----------------------------------------------------------------------------
# read conversion factor output and save as .Rdata
# -----------------------------------------------------------------------------

# recall: con_fac created in 03write_update_run_pwc.R
con_fac_output <- data.frame(con_fac)
dim(con_fac_output)

# update colnames
colnames(con_fac_output)
colnames(con_fac_output) <- c("Conv.Factor")

# write out
save(con_fac_output, file = paste(pwcdir,"io/con_fac_output.RData",sep = ""))
class(con_fac_output)





# ------------------------------------------------------------------------------
# create combined output of LHS input variables and MAX outputs
# ------------------------------------------------------------------------------


# ---------------------------------------------
# load LHS input file
# ---------------------------------------------

# recall : input_list is a uniformly distributed Latin hypercube 
indata <- read.csv(file = paste(pwcdir, "io/inputlist_przm_vvwm.csv", sep = ""), header = TRUE)

class(indata)
dim(indata)

# check class for each variable
for(i in 1:ncol(indata)){
  print(class(indata[,i]))
}

colnames(indata)
summary(indata)


# remove column "X"
inputs_lhs <- indata[,-1]

colnames(inputs_lhs)
dim(inputs_lhs)
summary(inputs_lhs)

# check class for each variable again
for(i in 1:ncol(inputs_lhs)){
  print(class(inputs_lhs[,i]))
}
colnames(inputs_lhs)


# ---------------------------------------------
# subset desired outputs - exclude 2009 due to transit
# ---------------------------------------------

# subset przm runoff 
przm_h2_output <- (outputdf[,4,1:Nsims]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
dim(przm_h2_output) #days*simulations
przm_h2_output <- przm_h2_output[732:2557,] # 2010 - 2014
# subset max przm H2O
przm_max_h2 <-apply(przm_h2_output, 2, function(x) max(x, na.rm = TRUE))


# subset przm pesticide in runoff
przm_pest_output <- (outputdf[,6,1:Nsims]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
dim(przm_pest_output) #days*simulations
przm_pest_output <- przm_pest_output[732:2557,] # 2010 - 2014
# subset max przm pest
przm_max_pest <-apply(przm_pest_output, 2, function(x) max(x, na.rm = TRUE))



# subset pwc Ave.Conc.H2O*1000000
pwc_h2_ugml <- (pwcoutdf[,2,1:Nsims]*1000000)#in ug/L
pwc_h2_ugml <- pwc_h2_ugml[732:2557,] #2010 - 2014
# calculate MAX 
pwc_max_h2 <-apply(pwc_h2_ugml, 2, function(x) max(x, na.rm = TRUE))
plot(pwc_max_h2)



# subset pwc Peak.Conc.H2O*1000000
pwc_peak_ugml <- (pwcoutdf[,4,1:Nsims]*1000000)#in ug/L
pwc_peak_ugml <- pwc_peak_ugml[732:2557,] #2010 - 2014
# calculate MAX 
pwc_max_peak <-apply(pwc_peak_ugml, 2, function(x) max(x, na.rm = TRUE))
plot(pwc_max_peak)



# subset benthic concentration
pwc_benthic <- (pwcoutdf[,3,1:Nsims]*1000000)#in ug/L   #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
pwc_benthic <- pwc_benthic[732:2557,] #2010 - 2014
# calculate MAX
pwc_max_benthic <-apply(pwc_benthic, 2, function(x) max(x, na.rm = TRUE))


# find sediment 
max_sed <- con_fac_output*pwc_max_benthic
max_sed <- as.numeric(unlist(max_sed))
typeof(max_sed)


# ---------------------------------------------
# combine all
# ---------------------------------------------

# combine LHS input variables with MAX outputs 
in_out_max <- cbind(inputs_lhs, przm_max_h2)
in_out_max <- cbind(in_out_max, pwc_max_peak)
in_out_max <- cbind(in_out_max, pwc_max_h2)
in_out_max <- cbind(in_out_max, pwc_max_benthic)
in_out_max <- cbind(in_out_max, con_fac_output)
in_out_max <- cbind(in_out_max, max_sed)
in_out_max <- cbind(in_out_max, przm_max_pest)

dim(in_out_max)

# check colnames
colnames(in_out_max)

# write out
write.csv(in_out_max, file = paste(pwcdir, "io/inputdata_przm_vvwm_max.csv", sep = ""))


# -----------------------------------------------------------------------------
# the end
# -----------------------------------------------------------------------------