# ------------------------------------------------------------------------------
# miscellaneous graphics
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# percentile plot: pwc Ave.Conc.H2O
# ------------------------------------------------------------------------------


# time subset (2009 - 2011) #367 - 1461
dim(pwc_h2_output) #days*sims
pwc_h2_time <- pwc_h2_output[367:1461,]

# create blank matrix to fill with percentiles
percent2.5 <- matrix(data = NA, nrow=dim(pwc_h2_time)[1], ncol=2)
percent97.5 <- matrix(data=NA, nrow=dim(pwc_h2_time)[1], ncol=2)

# compute percentiles
for (i in 1:dim(percent2.5)[1]){
  row_percent <- quantile(pwc_h2_time[i,], probs = .025, na.rm = T)
  
  percent2.5[i,1] <- row_percent
  percent2.5[i,2] <- i
}
percent2.5 <- as.data.frame(percent2.5)
names(percent2.5) <- c("percentile", "day")
percent2.5$percentile <- percent2.5$percentile*1000000 #convert units to ug/ml ?

for (i in 1:dim(percent97.5)[1]){
  row_percent <- quantile(pwc_h2_time[i,], probs = .975, na.rm = T)
  
  percent97.5[i,1] <- row_percent
  percent97.5[i,2] <- i
}
percent97.5 <- as.data.frame(percent97.5)
names(percent97.5) <- c("percentile", "day")
percent97.5$percentile <- percent97.5$percentile*1000000 #convert units to ug/ml ?


# plot
merge_pwc_percentile <- merge(percent2.5, percent97.5, by="day")
names(merge_pwc_percentile) <- c("day", "2.5", "97.5")
melt_pwc_percentile <- melt(merge_pwc_percentile, id.var="day")


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_pwc_ave_h2.png", sep=""),width=10, height=10, units="in",res=250) 

pwc_percentile_plot <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 1095))+ 
  theme_bw()+
  labs(title = "", x = "Day", y = "PWC Ave.Conc.H20", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")

print(pwc_percentile_plot)
dev.off()







# # ------------------------------------------------------------------------------
# # percentile plot: przm RUNF0
# # ------------------------------------------------------------------------------
# 
# 
# # time subset (2009 - 2011) #367 - 1461
# dim(przm_h2_output) #days*sims
# przm_h2_time <- przm_h2_output[367:1461,]
# 
# # create blank matrix to fill with percentiles
# percent2.5 <- matrix(data = NA, nrow=dim(przm_h2_time)[1], ncol=2)
# percent97.5 <- matrix(data=NA, nrow=dim(przm_h2_time)[1], ncol=2)
# 
# # compute percentiles
# for (i in 1:dim(percent2.5)[1]){
#   row_percent <- quantile(przm_h2_time[i,], probs = .025, na.rm = T)
#   
#   percent2.5[i,1] <- row_percent
#   percent2.5[i,2] <- i
# }
# percent2.5 <- as.data.frame(percent2.5)
# names(percent2.5) <- c("percentile", "day")
# percent2.5$percentile <- percent2.5$percentile*1000000 #convert units to ug/ml ?
# 
# for (i in 1:dim(percent97.5)[1]){
#   row_percent <- quantile(przm_h2_time[i,], probs = .975, na.rm = T)
#   
#   percent97.5[i,1] <- row_percent
#   percent97.5[i,2] <- i
# }
# percent97.5 <- as.data.frame(percent97.5)
# names(percent97.5) <- c("percentile", "day")
# percent97.5$percentile <- percent97.5$percentile*1000000 #convert units to ug/ml ?
# 
# 
# # plot
# merge_przm_percentile <- merge(percent2.5, percent97.5, by="day")
# names(merge_przm_percentile) <- c("day", "2.5", "97.5")
# melt_przm_percentile <- melt(merge_przm_percentile, id.var="day")
# 
# 
# # save figure as png
# png(filename= paste(pwcdir, "figures/percentile_przm_runf0.png", sep=""),width=10, height=10, units="in",res=250) 
# 
# przm_percentile_plot <- ggplot(melt_przm_percentile, aes(x=day, y=value, col=variable)) +
#   geom_line() +
#   scale_x_continuous(limits = c(0, 1095))+ 
#   theme_bw()+
#   labs(title = "", x = "Day", y = "PRZM RUNF0", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
#   theme(legend.position = "bottom")
# 
# dev.off()



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------