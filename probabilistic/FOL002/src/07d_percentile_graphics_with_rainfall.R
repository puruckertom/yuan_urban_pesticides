# ------------------------------------------------------------------------------
#percentile graphics with rainfall events
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# load data
# ------------------------------------------------------------------------------
library(gtable)

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
percentiles$percent.001 <- percentiles$percent.001*1000000 #convert units to ug/ml 
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
percentiles$deterministic <- determ$Ave.Conc.H20*1000000 #convert units to ug/ml



# --------------------------------
# plot percentile data
# --------------------------------

# set colors
sd1 <- "#08519c"
sd2 <- "#4292c6"
sd3 <- "#9ecae1"
med <- "#08306b"
det <- "#ef3b2c"

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# ---------------------------------
# plot precipitation data
# ---------------------------------

# read in weather file
precip <- read.table(file=paste(pwcdir_weather, "17484_grid_folsom.wea", sep=""), header=FALSE, sep=",")

colnames(precip) <- c("month", "day", "year", "precip_cm", "et_cm", "temp_c", "windspeed_cms", "solar_la")
precip$date <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")

precip_0914 <- precip[c(367:2557),]

p_0914 <- ggplot(precip_0914, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p_0914)



# --------------------------------
# plot together
# --------------------------------

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_09-14_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p_0914)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()




# ------------------------------------
# narrow in on time chunks
# ------------------------------------


# ---------------
# 2009
# ---------------
# plot percentiles
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2009-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(367:731),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)



# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_09_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()



# ---------------
# 2010
# ---------------
# plot percentiles
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2010-01-01', '2010-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(732:1096),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_10_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()

# ---------------
# 2011
# ---------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2011-01-01', '2011-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1097:1461),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_11_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()



# ---------------
# 2012
# ---------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1462:1827),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_12_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()



# ---------------
# 2013
# ---------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1828:2192),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_13_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# ---------------
# 2014
# ---------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2014-01-01', '2014-12-31'))) +
  scale_y_continuous(limits=c(0, 0.015)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(2193:2557),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_14_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
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
percentiles$percent.001 <- percentiles$percent.001*1000000 #convert units to ug/ml 
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
percentiles$deterministic <- determ$Ave.Conc.benth*1000000 #convert units to ug/ml



# --------------------------------
# plot percentiles
# --------------------------------

# set colors
sd1 <- "#6a51a3"
sd2 <- "#807dba"
sd3 <- "#bcbddc"
med <- "#3f007d"
det <- "#ef3b2c"

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# ---------------------------------
# plot precipitation data
# ---------------------------------

# read in weather file
precip_0914 <- precip[c(367:2557),]

p_0914 <- ggplot(precip_0914, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p_0914)

# --------------------------------
# plot together
# --------------------------------
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_09-14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p_0914)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# narrow in on time chunks

# --------------
# 2009
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2009-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(367:731),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)



# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_09_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()



# --------------
# 2010
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2010-01-01', '2010-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(732:1096),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_10_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()

# --------------
# 2011
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2011-01-01', '2011-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1097:1461),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_11_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# --------------
# 2012
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1462:1827),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_12_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# --------------
# 2013
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(1828:2192),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_13_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# --------------
# 2014
# --------------
# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  geom_line(aes(y=percent.5, color="Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2014-01-01', '2014-12-31'))) +
  scale_y_continuous(limits=c(0, 0.006)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("Median" =med, "Deterministic"=det))

# plot precip
precip_sub <- precip[c(2193:2557),]

p <- ggplot(precip_sub, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(p)

# plot together
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_rainfall_14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p)
g3 <- ggplotGrob(pwc_pplot)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------