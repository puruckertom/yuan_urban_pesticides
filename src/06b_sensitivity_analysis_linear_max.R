# ------------------------------------------------------------------------------
# Run sensitivity analysis with max daily concentrations
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


# ------------------------------------------------------------------------------
# pcc
# ------------------------------------------------------------------------------


# pcc -- pwc_max_h2
for(i in 1:ncol(inputs_lhs)){
  var <- colnames(inputs_lhs)[i]
  
  pcor_value <- pcor(cbind(inputs_lhs[,i],pwc_max_h2), method="pearson")$estimate[1,2]
  
  print(paste(var, pcor_value, min(inputs_lhs[,i]), max(inputs_lhs[,i])))
}



# pcc -- for all model outputs
przm_max_h2_pcc <- pcc(inputs_lhs, przm_max_h2, rank = F)
pwc_max_peak_pcc <- pcc(inputs_lhs, pwc_max_peak, rank = F)
pwc_max_h2_pcc <- pcc(inputs_lhs, pwc_max_h2, rank = F)
pwc_max_benthic_pcc <- pcc(inputs_lhs, pwc_max_benthic, rank = F)
max_sed_pcc <- pcc(inputs_lhs, max_sed, rank = F)



# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))
plot(przm_max_h2_pcc, ylim = c(-1,1),main="PRZM")
plot(pwc_max_h2_pcc, ylim = c(-1,1),main="H20")

plot(pwc_max_benthic_pcc, ylim = c(-1,1),main="Benthic")
plot(max_sed_pcc, ylim = c(-1,1),main="Sediment")




# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------