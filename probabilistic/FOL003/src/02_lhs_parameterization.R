# ------------------------------------------------------------------
# Latin Hypercube sampling of parameters
# ------------------------------------------------------------------

# state the number of simulations 
Nsims <- 5000

# weather file - must be .wea
pwc_weather_used <- "17484_grid_folsom.wea"
pwc_weather <- paste(pwcdir_weather, pwc_weather_used, sep="")

# state the simulation start and end
#must have mm/dd/yyyy format
simstart <- "01/01/2008"
simend <- "12/31/2014"

# list input paramters
input_parameters <- c("PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                      "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq",
                      "temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro","SOL",
                      "benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

# import paramter range csv table
param_ranges <- read.csv(paste(pwcdir,"input/lhs/lhs_param_ranges.csv",sep=""),header= TRUE, sep= ",", stringsAsFactors = FALSE, row.names=NULL)



# ------------------------------------------------------------------
# LHS
# ------------------------------------------------------------------

# conduct Latin Hypercube Sampling for each input parameter
quantile_list <- randomLHS(Nsims, length(input_parameters)) #(number of simulations, number of var)

dimnames(quantile_list) <- list(NULL, input_parameters) 
head(quantile_list)

# set-up a dataframe to store the randomly sampled parameter values
input_list <- data.frame(matrix(NA,nrow=dim(quantile_list)[1],ncol=dim(quantile_list)[2])) 

# for each parameter, sample from uniform distr within parameter's bounds
for(i in 1:ncol(quantile_list)){
  #create quasi random numbers
  draw <- round(qunif(quantile_list[,i], min = param_ranges[i,2], max = param_ranges[i,3]),3)
  input_list[,i] <- draw
}

# input_list is now a uniformly distributed Latin hypercube 
input_list
colnames(input_list) <- c(input_parameters) 
head(input_list)

# show the parameters are sampled from a uniform distribution - histogram, plot examples
hist(input_list$PFAC,main="Histogram", xlab="PFAC", border="darkblue", col="gray")
plot(input_list$CN_c, pch = 19, col = "orange")

# overwrite DSRATE to be the same as the randomly generated DWRATE
input_list[,"DSRATE"] <- input_list[,"DWRATE"]

# write out the file
write.csv(input_list, file = paste(pwcdir, "io/inputlist_przm_vvwm.csv", sep = ""))



# --------------------------------------------------------------------
# the end
# --------------------------------------------------------------------