# ------------------------------------------------------------------------------
# percentile graphics
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# load data
# ------------------------------------------------------------------------------

# pwc output array
# recall: this was created in 03write_update_run_pwc and saved in 05_write_output_into_df
# recall: this is an array of all of the output_FOL002_parent_only_Custom_Parent_daily.csv files 
load(paste(pwcdir, "io/pwcout.RData", sep = ""))
dim(pwcoutdf)

# subset Ave.Conc.H2O  
pwc_h2_output <- pwcoutdf[,2,1:Nsims] #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
dim(pwc_h2_output) #days*simulations

# subset Ave.Conc.benth 
pwc_ben_output <- pwcoutdf[,3,1:Nsims] #1depth, 2Ave.Conc.H20, 3Ave.Conc.benth, 4Peak.Conc.H20
dim(pwc_ben_output) #days*simulations


# przm output array
# recall: this was created in 03write_update_run_pwc and saved in 05_write_output_into_df
# recall: this is an array of all of the output.zts files  (dim = num_of_days*output_cols*sims)
load(paste(pwcdir, "io/przmout.RData", sep = ""))
dim(outputdf)

# subset RUNF0
przm_h2_output <- outputdf[,4,1:Nsims] #"YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0"
dim(przm_h2_output) #days*simulations




# ------------------------------------------------------------------------------
# percentile plot: pwc Ave.Conc.H2O
# ------------------------------------------------------------------------------


# --------------------------------
# data set-up
# --------------------------------

dim(pwc_h2_output) #days*sims

# create blank matrix to fill with percentiles
percentiles <- matrix(data=NA, nrow=dim(pwc_h2_output)[1], ncol=8)
colnames(percentiles) <- c("day", "percent.001", "percent.023", "percent.159", "percent.5",
                           "percent.841", "percent.977", "percent.999")
percentiles <- as.data.frame(percentiles)

# date format
percentiles$day <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")


# compute percentiles
for (i in 1:dim(percentiles)[1]){
  p001 <- quantile(pwc_h2_output[i,], probs=.001, na.rm=T)
  percentiles[i,2] <- p001
  
  p023 <- quantile(pwc_h2_output[i,], probs=.023, na.rm=T)
  percentiles[i,3] <- p023
  
  p159 <- quantile(pwc_h2_output[i,], probs=.159, na.rm=T)
  percentiles[i,4] <- p159
  
  p5 <- quantile(pwc_h2_output[i,], probs=.5, na.rm=T)
  percentiles[i,5] <- p5
  
  p841 <- quantile(pwc_h2_output[i,], probs=.841, na.rm=T)
  percentiles[i,6] <- p841
  
  p977 <- quantile(pwc_h2_output[i,], probs=.977, na.rm=T)
  percentiles[i,7] <- p977
  
  p999 <- quantile(pwc_h2_output[i,], probs=.999, na.rm=T)
  percentiles[i,8] <- p999
}
percentiles$percent.001 <- percentiles$percent.001*1000000  #convert units to ug/L 
percentiles$percent.023 <- percentiles$percent.023*1000000 
percentiles$percent.159 <- percentiles$percent.159*1000000 
percentiles$percent.5 <- percentiles$percent.5*1000000 
percentiles$percent.841 <- percentiles$percent.841*1000000   
percentiles$percent.977 <- percentiles$percent.977*1000000  
percentiles$percent.999 <- percentiles$percent.999*1000000  



# read in deterministic output
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/outputs/output_FOL002_parent_only_Custom_Parent_daily.csv",
                   header= FALSE, sep= ",", skip = 5, stringsAsFactors = FALSE, row.names=NULL)
colnames(determ) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")
determ <- as.data.frame(determ)

# subset Ave.conc.H20, add to percentiles df
percentiles$deterministic <- determ$Ave.Conc.H20*1000000 #convert units to ug/L



# impose a false zero
for (i in 1:dim(percentiles)[1]){
  if (percentiles[i,2] < 1e-8){
    percentiles[i,2] <- 1e-8
  } 
} 


# --------------------------------
# plot
# --------------------------------

# set colors
sd1 <- "#08519c"
sd2 <- "#4292c6"
sd3 <- "#9ecae1"
med <- "#78c679"
det <- "#ef3b2c"

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_09-14_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  scale_x_date(date_breaks="1 year", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# narrow in on time chunks (by year)
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_09_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2009-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_10_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2010-01-01', '2010-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_11_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2011-01-01', '2011-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_12_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()



# save figure as png
png(filename= paste(pwcdir, "figures/percentile_13_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()



# save figure as png
png(filename= paste(pwcdir, "figures/percentile_14_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2014-01-01', '2014-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Water Columm", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()






# ------------------------------------------------------------------------------
# percentile plot: pwc Ave.Conc.benth
# ------------------------------------------------------------------------------


# --------------------------------
# data set-up
# --------------------------------

dim(pwc_ben_output) #days*sims

# create blank matrix to fill with percentiles
percentiles <- matrix(data=NA, nrow=dim(pwc_ben_output)[1], ncol=8)
colnames(percentiles) <- c("day", "percent.001", "percent.023", "percent.159", "percent.5",
                           "percent.841", "percent.977", "percent.999")
percentiles <- as.data.frame(percentiles)

# date format
percentiles$day <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")


# compute percentiles
for (i in 1:dim(percentiles)[1]){
  p001 <- quantile(pwc_ben_output[i,], probs=.001, na.rm=T)
  percentiles[i,2] <- p001
  
  p023 <- quantile(pwc_ben_output[i,], probs=.023, na.rm=T)
  percentiles[i,3] <- p023
  
  p159 <- quantile(pwc_ben_output[i,], probs=.159, na.rm=T)
  percentiles[i,4] <- p159
  
  p5 <- quantile(pwc_ben_output[i,], probs=.5, na.rm=T)
  percentiles[i,5] <- p5
  
  p841 <- quantile(pwc_ben_output[i,], probs=.841, na.rm=T)
  percentiles[i,6] <- p841
  
  p977 <- quantile(pwc_ben_output[i,], probs=.977, na.rm=T)
  percentiles[i,7] <- p977
  
  p999 <- quantile(pwc_ben_output[i,], probs=.999, na.rm=T)
  percentiles[i,8] <- p999
}
percentiles$percent.001 <- percentiles$percent.001*1000000 #convert units to ug/L 
percentiles$percent.023 <- percentiles$percent.023*1000000 
percentiles$percent.159 <- percentiles$percent.159*1000000 
percentiles$percent.5 <- percentiles$percent.5*1000000 
percentiles$percent.841 <- percentiles$percent.841*1000000  
percentiles$percent.977 <- percentiles$percent.977*1000000  
percentiles$percent.999 <- percentiles$percent.999*1000000  




# read in deterministic output
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/outputs/output_FOL002_parent_only_Custom_Parent_daily.csv",
                   header= FALSE, sep= ",", skip = 5, stringsAsFactors = FALSE, row.names=NULL)
colnames(determ) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")
determ <- as.data.frame(determ)

# subset Ave.conc.H20, add to percentiles df
percentiles$deterministic <- determ$Ave.Conc.benth*1000000 #convert units to ug/L


# impose a false zero
for (i in 1:dim(percentiles)[1]){
  if (percentiles[i,2] < 1e-8){
    percentiles[i,2] <- 1e-8
  } 
} 


# --------------------------------
# plot
# --------------------------------

# set colors
sd1 <- "#6a51a3"
sd2 <- "#807dba"
sd3 <- "#bcbddc"
med <- "#78c679"
det <- "#ef3b2c"

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_09-14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# narrow in on time chunks (by year)
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_09_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2009-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_10_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2010-01-01', '2010-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_11_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2011-01-01', '2011-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()


# save figure as png
png(filename= paste(pwcdir, "figures/percentile_12_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()



# save figure as png
png(filename= paste(pwcdir, "figures/percentile_13_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()



# save figure as png
png(filename= paste(pwcdir, "figures/percentile_14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2014-01-01', '2014-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) +
  labs(title = "Daily Average Aqueous Bifenthrin Concentration in Benthic Zone", x = "", y = "Bifenthrin Concentration (ug/L) (log10)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))
print(pwc_pplot)
dev.off()



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------