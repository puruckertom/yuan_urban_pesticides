# ------------------------------------------------------------------------------
# miscellaneous graphics
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
percent2.5 <- matrix(data = NA, nrow=dim(pwc_h2_output)[1], ncol=2)
percent97.5 <- matrix(data=NA, nrow=dim(pwc_h2_output)[1], ncol=2)

# compute percentiles
for (i in 1:dim(percent2.5)[1]){
  row_percent <- quantile(pwc_h2_output[i,], probs = .025, na.rm = T)
  
  percent2.5[i,1] <- row_percent
  percent2.5[i,2] <- i
}
percent2.5 <- as.data.frame(percent2.5)
names(percent2.5) <- c("percentile", "day")
percent2.5$percentile <- percent2.5$percentile*1000000 #convert units to ug/ml ?

for (i in 1:dim(percent97.5)[1]){
  row_percent <- quantile(pwc_h2_output[i,], probs = .975, na.rm = T)
  
  percent97.5[i,1] <- row_percent
  percent97.5[i,2] <- i
}
percent97.5 <- as.data.frame(percent97.5)
names(percent97.5) <- c("percentile", "day")
percent97.5$percentile <- percent97.5$percentile*1000000 #convert units to ug/ml ?


# read in deterministic output
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/December-2016-FOL002_parent_only_FOL002_Custom_Parent_daily.csv",
                  header= FALSE, sep= ",", skip = 5, stringsAsFactors = FALSE, row.names=NULL)
colnames(determ) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")
determ <- as.data.frame(determ)

# subset Ave.conc.H20
ave_h2 <- determ[, 2]
ave_h2_ugml <- ave_h2*1000000 #convert units to ug/ml ?
day <- c(1:2557)
determ_ave_h2 <- cbind(ave_h2_ugml, day)
colnames(determ_ave_h2) <- c("deterministic", "day")
dim(determ_ave_h2)


# merge and melt percentiles and deterministic
merge_pwc_percentile <- merge(percent2.5, percent97.5, by="day")
names(merge_pwc_percentile) <- c("day", "2.5", "97.5")
dim(merge_pwc_percentile)

merge_pwc_percentile2 <- merge(merge_pwc_percentile, determ_ave_h2, by="day")
dim(merge_pwc_percentile2)
merge_pwc_percentile2$day <-seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days") #change days to format 1999-01-01

melt_pwc_percentile <- melt(merge_pwc_percentile2, id.var="day")

# --------------------------------
# plot
# --------------------------------
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_08-14_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 
# plot
pwc_percentile_plot <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 year", date_labels="%m/%d/%y")+ 
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot)
dev.off()




# narrow in on specific time chunks (yearly)

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_13_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 
pwc_percentile_plot_13 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31')))+  
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot_13)
dev.off()

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_12_pwc_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 
pwc_percentile_plot_12 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31')))+  
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Water Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot_12)
dev.off()




# ------------------------------------------------------------------------------
# percentile plot: pwc Ave.Conc.benth
# ------------------------------------------------------------------------------


# --------------------------------
# data set-up
# --------------------------------

dim(pwc_ben_output) #days*sims

# create blank matrix to fill with percentiles
percent2.5 <- matrix(data = NA, nrow=dim(pwc_ben_output)[1], ncol=2)
percent97.5 <- matrix(data=NA, nrow=dim(pwc_ben_output)[1], ncol=2)

# compute percentiles
for (i in 1:dim(percent2.5)[1]){
  row_percent <- quantile(pwc_ben_output[i,], probs = .025, na.rm = T)
  
  percent2.5[i,1] <- row_percent
  percent2.5[i,2] <- i
}
percent2.5 <- as.data.frame(percent2.5)
names(percent2.5) <- c("percentile", "day")
percent2.5$percentile <- percent2.5$percentile*1000000 #convert units to ug/ml ?

for (i in 1:dim(percent97.5)[1]){
  row_percent <- quantile(pwc_ben_output[i,], probs = .975, na.rm = T)
  
  percent97.5[i,1] <- row_percent
  percent97.5[i,2] <- i
}
percent97.5 <- as.data.frame(percent97.5)
names(percent97.5) <- c("percentile", "day")
percent97.5$percentile <- percent97.5$percentile*1000000 #convert units to ug/ml ?


# read in deterministic output
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/December-2016-FOL002_parent_only_FOL002_Custom_Parent_daily.csv",
                   header= FALSE, sep= ",", skip = 5, stringsAsFactors = FALSE, row.names=NULL)
colnames(determ) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")
determ <- as.data.frame(determ)

# subset Ave.conc.benth
ave_ben <- determ[, 3]
ave_ben_ugml <- ave_ben*1000000 #convert units to ug/ml ?
day <- c(1:2557)
determ_ave_ben <- cbind(ave_ben_ugml, day)
colnames(determ_ave_ben) <- c("deterministic", "day")
dim(determ_ave_ben)


# merge and melt percentiles and deterministic
merge_pwc_percentile <- merge(percent2.5, percent97.5, by="day")
names(merge_pwc_percentile) <- c("day", "2.5", "97.5")
dim(merge_pwc_percentile)

merge_pwc_percentile2 <- merge(merge_pwc_percentile, determ_ave_ben, by="day")
dim(merge_pwc_percentile2)
merge_pwc_percentile2$day <-seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days") #change days to format 1999-01-01

melt_pwc_percentile <- melt(merge_pwc_percentile2, id.var="day")

# --------------------------------
# plot
# --------------------------------
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_08-14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 
# plot
pwc_percentile_plot <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 year", date_labels="%m/%d/%y")+ 
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot)
dev.off()




# narrow in on specific time chunks (yearly)
# save figure as png
png(filename= paste(pwcdir, "figures/percentile_14_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 
pwc_percentile_plot_14 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2014-01-01', '2014-12-31')))+  
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot_14)
dev.off()

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_13_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 
pwc_percentile_plot_13 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31')))+  
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot_13)
dev.off()

# save figure as png
png(filename= paste(pwcdir, "figures/percentile_12_pwc_ave_benthic.png", sep=""),width=20, height=10, units="in",res=300) 
pwc_percentile_plot_12 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_breaks="1 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31')))+  
  theme_bw()+
  scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
  labs(title = "", x = "", y = "Bifenthrin Concentration in Pool Benthic Column (ug/ml)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(pwc_percentile_plot_12)
dev.off()



# # # -----------------------------------------------------------------------------
# # # percentile plot: przm RUNF0
# # # -----------------------------------------------------------------------------
# 
# 
# # --------------------------------
# # data set-up
# # --------------------------------
# 
# dim(przm_h2_output) #days*sims
# 
# # create blank matrix to fill with percentiles
# percent2.5 <- matrix(data = NA, nrow=dim(przm_h2_output)[1], ncol=2)
# percent97.5 <- matrix(data=NA, nrow=dim(przm_h2_output)[1], ncol=2)
# 
# # compute percentiles
# for (i in 1:dim(percent2.5)[1]){
#   row_percent <- quantile(przm_h2_output[i,], probs = .025, na.rm = T)
#   
#   percent2.5[i,1] <- row_percent
#   percent2.5[i,2] <- i
# }
# percent2.5 <- as.data.frame(percent2.5)
# names(percent2.5) <- c("percentile", "day")
# 
# for (i in 1:dim(percent97.5)[1]){
#   row_percent <- quantile(przm_h2_output[i,], probs = .975, na.rm = T)
#   
#   percent97.5[i,1] <- row_percent
#   percent97.5[i,2] <- i
# }
# percent97.5 <- as.data.frame(percent97.5)
# names(percent97.5) <- c("percentile", "day")
# 
# 
# # read in deterministic output
# determ <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/December-2016-FOL002_parent_only.zts",
#                    header= FALSE, sep= "", skip = 3, stringsAsFactors = FALSE, row.names=NULL)
# colnames(determ) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")
# determ <- as.data.frame(determ)
# 
# # subset RUNF0
# runf <- determ[, 4]
# day <- c(1:2557)
# determ_runf <- cbind(runf, day)
# colnames(determ_runf) <- c("deterministic", "day")
# dim(determ_runf)
# 
# 
# # merge and melt percentiles and deterministic
# merge_pwc_percentile <- merge(percent2.5, percent97.5, by="day")
# names(merge_pwc_percentile) <- c("day", "2.5", "97.5")
# dim(merge_pwc_percentile)
# 
# merge_pwc_percentile2 <- merge(merge_pwc_percentile, determ_runf, by="day")
# dim(merge_pwc_percentile2)
# merge_pwc_percentile2$day <-seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days") #change days to format 1999-01-01
# 
# melt_pwc_percentile <- melt(merge_pwc_percentile2, id.var="day")
# 
# # --------------------------------
# # plot
# # --------------------------------
# # save figure as png
# png(filename= paste(pwcdir, "figures/percentile_08-14_przm_runf.png", sep=""),width=20, height=10, units="in",res=300) 
# # plot
# pwc_percentile_plot <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
#   geom_line() +
#   scale_x_date(date_breaks="1 year", date_labels="%m/%d/%y")+ 
#   theme_bw()+
#   scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
#   labs(title = "", x = "", y = "Bifenthrin in Runoff", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
#   theme(legend.position = "bottom")
# print(pwc_percentile_plot)
# dev.off()
# 
# 
# 
# 
# # narrow in on specific time chunks (yearly)
# 
# # save figure as png
# png(filename= paste(pwcdir, "figures/percentile_13_przm_runf.png", sep=""),width=20, height=10, units="in",res=300) 
# pwc_percentile_plot_13 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
#   geom_line() +
#   scale_x_date(date_breaks="2 months", date_labels="%m-%d-%y", limits=as.Date(c('2013-01-01', '2013-12-31')))+  
#   theme_bw()+
#   scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
#   labs(title = "", x = "", y = "Bifenthrin in Runoff", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
#   theme(legend.position = "bottom")
# print(pwc_percentile_plot_13)
# dev.off()
# 
# # save figure as png
# png(filename= paste(pwcdir, "figures/percentile_12_przm_runf.png", sep=""),width=20, height=10, units="in",res=300) 
# pwc_percentile_plot_12 <- ggplot(melt_pwc_percentile, aes(x=day, y=value, col=variable)) +
#   geom_line() +
#   scale_x_date(date_breaks="2 months", date_labels="%m-%d-%y", limits=as.Date(c('2012-01-01', '2012-12-31')))+  
#   theme_bw()+
#   scale_color_manual(values=c("#43a2ca", "#43a2ca", "#f03b20")) +
#   labs(title = "", x = "", y = "Bifenthrin in Runoff", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
#   theme(legend.position = "bottom")
# print(pwc_percentile_plot_12)
# dev.off()


# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------