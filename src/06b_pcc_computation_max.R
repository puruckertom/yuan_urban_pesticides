# ------------------------------------------------------------------------------
# Computation of PCC for max concentrations
# ------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# read files, call variables
# -------------------------------------------------------------------------

# recall : in_out_max was created in 05_write_output_into_df.R 
in_out_max <- read.csv(file = paste(pwcdir, "io/inputdata_przm_vvwm_max.csv", sep = ""), header = TRUE)

class(in_out_max)
dim(in_out_max) 
names(in_out_max)

# call variables
przm_max_h2 <- in_out_max$przm_max_h2
pwc_max_h2 <- in_out_max$pwc_max_h2
pwc_max_peak <- in_out_max$pwc_max_peak
pwc_max_benthic <- in_out_max$pwc_max_benthic
max_sed <- in_out_max$max_sed


# recall: inputs_lhs contain the LHS input variables; edited in 05_write_output_into_df.R
dim(inputs_lhs)
nvars <- length(inputs_lhs)


# ------------------------------------------------------------------------------
# pcc -- pwc_max_h2
# ------------------------------------------------------------------------------

# create BLANK matrix for PCC values
pcc_pwc_max_h2<- matrix(data=NA, nrow=nvars, ncol=2)
dim(pcc_pwc_max_h2) #variables*columns
pcc_pwc_max_h2 <- as.data.frame(pcc_pwc_max_h2)
names(pcc_pwc_max_h2) <- c("var", "pcc")

# compute pcc
pwc_max_h2_pcc <- pcc(inputs_lhs, pwc_max_h2, rank = F)
for (i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcc_pwc_max_h2[i,1] <- var
  pcc_pwc_max_h2[i,2] <- pwc_max_h2_pcc$PCC[[i, 1]]
}


# write out pcc results
dim(pcc_pwc_max_h2)

save(pcc_pwc_max_h2,file = paste(pwcdir,"io/pcc_pwc_max_h2.RData", sep = ""))
write.csv(pcc_pwc_max_h2, file = paste(pwcdir, "io/pcc_pwc_max_h2.csv", sep = ""))



# ------------------------------------------------------------------------------
# pcc -- pwc_max_peak
# ------------------------------------------------------------------------------

# create BLANK matrix for PCC values
pcc_max_peak<- matrix(data=NA, nrow=nvars, ncol=2)
dim(pcc_max_peak) #variables*columns
pcc_max_peak <- as.data.frame(pcc_max_peak)
names(pcc_max_peak) <- c("var", "pcc")

# compute pcc
pwc_max_peak_pcc <- pcc(inputs_lhs, pwc_max_peak, rank = F)
for (i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcc_max_peak[i,1] <- var
  pcc_max_peak[i,2] <- pwc_max_peak_pcc$PCC[[i, 1]]
}


# write out pcc results
dim(pcc_max_peak)

save(pcc_max_peak,file = paste(pwcdir,"io/pcc_pwc_max_peak.RData", sep = ""))
write.csv(pcc_max_peak, file = paste(pwcdir, "io/pcc_pwc_max_peak.csv", sep = ""))




# ------------------------------------------------------------------------------
# pcc -- przm_max_h2
# ------------------------------------------------------------------------------

# create BLANK matrix for PCC values
pcc_przm_max_h2<- matrix(data=NA, nrow=nvars, ncol=2)
dim(pcc_przm_max_h2) #variables*columns
pcc_przm_max_h2 <- as.data.frame(pcc_przm_max_h2)
names(pcc_przm_max_h2) <- c("var", "pcc")

# compute pcc
przm_max_h2_pcc <- pcc(inputs_lhs, przm_max_h2, rank = F)
for (i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcc_przm_max_h2[i,1] <- var
  pcc_przm_max_h2[i,2] <- przm_max_h2_pcc$PCC[[i, 1]]
}


# write out pcc results
dim(pcc_przm_max_h2)

save(pcc_przm_max_h2,file = paste(pwcdir,"io/pcc_przm_max_h2.RData", sep = ""))
write.csv(pcc_przm_max_h2, file = paste(pwcdir, "io/pcc_przm_max_h2.csv", sep = ""))




# ------------------------------------------------------------------------------
# pcc -- pwc_max_benthic
# ------------------------------------------------------------------------------

# create BLANK matrix for PCC values
pcc_pwc_max_benthic<- matrix(data=NA, nrow=nvars, ncol=2)
dim(pcc_pwc_max_benthic) #variables*columns
pcc_pwc_max_benthic <- as.data.frame(pcc_pwc_max_benthic)
names(pcc_pwc_max_benthic) <- c("var", "pcc")

# compute pcc
pwc_max_benthic_pcc <- pcc(inputs_lhs, pwc_max_benthic, rank = F)
for (i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcc_pwc_max_benthic[i,1] <- var
  pcc_pwc_max_benthic[i,2] <- pwc_max_benthic_pcc$PCC[[i, 1]]
}


# write out pcc results
dim(pcc_pwc_max_benthic)

save(pcc_pwc_max_benthic,file = paste(pwcdir,"io/pcc_pwc_max_benthic.RData", sep = ""))
write.csv(pcc_pwc_max_benthic, file = paste(pwcdir, "io/pcc_pwc_max_benthic.csv", sep = ""))



# ------------------------------------------------------------------------------
# pcc -- max_sed
# ------------------------------------------------------------------------------

# create BLANK matrix for PCC values
pcc_max_sed<- matrix(data=NA, nrow=nvars, ncol=2)
dim(pcc_max_sed) #variables*columns
pcc_max_sed <- as.data.frame(pcc_max_sed)
names(pcc_max_sed) <- c("var", "pcc")

# compute pcc
max_sed_pcc <- pcc(inputs_lhs, max_sed, rank = F)
for (i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcc_max_sed[i,1] <- var
  pcc_max_sed[i,2] <- max_sed_pcc$PCC[[i, 1]]
}


# write out pcc results
dim(pcc_max_sed)

save(pcc_max_sed,file = paste(pwcdir,"io/pcc_max_sed.RData", sep = ""))
write.csv(pcc_max_sed, file = paste(pwcdir, "io/pcc_max_sed.csv", sep = ""))




# ------------------------------------------------------------------------------
# quick plot
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))
plot(przm_max_h2_pcc, ylim = c(-1,1),main="PRZM")
abline(h=0.2)
abline(h = -0.2)

plot(pwc_max_h2_pcc, ylim = c(-1,1),main="H20")
abline(h=0.2)
abline(h = -0.2)

plot(pwc_max_benthic_pcc, ylim = c(-1,1),main="Benthic")
abline(h=0.2)
abline(h = -0.2)

plot(max_sed_pcc, ylim = c(-1,1),main="Sediment")
abline(h=0.2)
abline(h = -0.2)



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------