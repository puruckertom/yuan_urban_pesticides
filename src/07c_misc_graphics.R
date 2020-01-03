# ------------------------------------------------------------------------------
# miscellaneous graphics
# ------------------------------------------------------------------------------

# time series of percentiles

# 2.5 percentile
# 97.5 percentile
quants <- quantile(x, probs=c(.025, .975), na.rm = T)
quants[[1]]
quants[[2]]


# przm RUNF0
przm_h2_output #days*sims

przm_h2_2 <- apply(przm_h2_output, 1, quantile, probs=.025, na.rm=T)
przm_h2_975 <- apply(przm_h2_output, 1, quantile, probs=.975, na.rm=T)


# pwc Ave.Conc.H2O  
pwc_h2_output #days*sims


# pwc Peak.Conc.H2O  
pwc_peak_output #days*sims


# pwc Ave.Conc.benth 
pwc_ben_output #days*sims




# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------