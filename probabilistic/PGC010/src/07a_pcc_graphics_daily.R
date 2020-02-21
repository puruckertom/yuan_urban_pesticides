# ------------------------------------------------------------------------------
# Graphics of daily PCC concentrations
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------
# load data, call variables
# -----------------------------------------------------------------------

ndays <- length(timearray)

date <- 367:ndays # might need to change.  first 366 days have NA pcc (because model output = 0)


# pcc daily results - Ave.Conc.H20
load(paste(pwcdir, "io/tarray_pcc_ave_conc_daily.RData", sep = ""))
dim(tarray_pcc_ave_conc_daily)

colnames(tarray_pcc_ave_conc_daily) <- colnames(inputs_lhs)
colnames(tarray_pcc_ave_conc_daily) 


# pcc daily results - Peak.Conc.H20
load(paste(pwcdir, "io/tarray_pcc_peak_daily.RData", sep = ""))
dim(tarray_pcc_peak_daily)

colnames(tarray_pcc_peak_daily) <- colnames(inputs_lhs)
colnames(tarray_pcc_peak_daily) 

# pcc daily results - Ave.Conc.benth
load(paste(pwcdir, "io/tarray_pcc_benthic_daily.RData", sep = ""))
dim(tarray_pcc_benthic_daily)

colnames(tarray_pcc_benthic_daily) <- colnames(inputs_lhs)
colnames(tarray_pcc_benthic_daily) 



# multiplot function
source(paste(pwcdir, "src/function_multiplot.R", sep=""))



# -----------------------------------------------------------------------
# plots 
# -----------------------------------------------------------------------


# ------------------------------------
# plot daily PCC (for model output = Ave.Conc.H20)  
# ------------------------------------

pcc_day_ave_conc <- as.data.frame(cbind(date, tarray_pcc_ave_conc_daily[367:ndays,1:dim(tarray_pcc_ave_conc_daily)[2]]))
colnames(pcc_day_ave_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                       "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                       "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

pcc_day_ave_conc$date2 <-seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="days")#format 1961-01-01

cont <- pcc_day_ave_conc%>% dplyr::select(one_of(c("date2","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                         "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                         "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date2")
melted_pwc <- na.omit(melted_pwc)


# PCC Time Series plots (all variables)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_all_ts_pwc_daily_ave_h2.png", sep=""),width=10, height=10, units="in",res=250) 

daily_pcc_ave_conc <- ggplot(melted_pwc, aes(x=date2, y=value, group=variable)) +
  geom_line() +
  facet_wrap(~variable, scale="free")+
  guides(fill=FALSE) +  
  ggtitle("Daily PCC for All Parameters with Bifenthrin Concentration in Water Column (2009 - 2014)")+
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  #annotate("text", x = 1000, y = 0.92, label = "Ave.Conc.H20", size=6) +
  theme_bw() +
  #scale_x_discrete(breaks = c(426,610,791,1035)) +
  scale_y_continuous(limits=c(min(melted_pwc$value), max(melted_pwc$value))) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())

print(daily_pcc_ave_conc)
dev.off()



# ------------------------------------
# plot daily PCC for highly sensitive parameters only (model output = Ave.Conc.H20)
# ------------------------------------

cont1<- pcc_day_ave_conc%>%select(one_of(c("date2","CN_c","uslels", "kd", "anae_aq", "FROC2")))

melted_pwc1 = melt(cont1, id.vars="date2")
melted_pwc1<- na.omit(melted_pwc1)


# -------------------
# 2009 - 2014
# -------------------

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_sensitive_ts_pwc_daily_ave_h2.png", sep=""),width=10, height=10, units="in",res=250) 

# selected days 367 - 2557 
daily_pcc_ave_conc_high <- ggplot(melted_pwc1, aes(x=date2, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "Daily PCC for Sensitive Parameters with Bifenthrin Concentration in Water Column (2009 - 2014)", x = "Day", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")

print(daily_pcc_ave_conc_high)
dev.off()



# ------------------------------------
# plot daily PCC (for model output = Ave.Conc.benth)  
# ------------------------------------

pcc_day_ben_conc <- as.data.frame(cbind(date, tarray_pcc_benthic_daily[367:ndays,1:dim(tarray_pcc_benthic_daily)[2]]))
colnames(pcc_day_ben_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                 "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")
pcc_day_ben_conc$date2 <-seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="days")#format 1961-01-01

cont <- pcc_day_ben_conc%>% dplyr::select(one_of(c("date2","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                                    "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                                    "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date2")
#melted_pwc <- na.omit(melted_pwc)


# PCC Time Series plots (all variables)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_all_ts_pwc_daily_benthic.png", sep=""),width=10, height=10, units="in",res=250) 

daily_pcc_ben_conc <- ggplot(melted_pwc, aes(x=date2, y=value, group=variable)) +
  geom_line() +
  facet_wrap(~variable, scale="free")+
  guides(fill=FALSE) +  
  ggtitle("Daily PCC for All Parameters with Bifenthrin Concentration in Benthic Zone (2009 - 2014)")+
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  #annotate("text", x = 1000, y = 0.92, label = "Ave.Conc.Benthic", size=6) +
  theme_bw() +
  scale_x_discrete(breaks = c(61,426,610,791,1035)) +
  scale_y_continuous(limits=c(min(melted_pwc$value), max(melted_pwc$value))) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())
print(daily_pcc_ben_conc)
dev.off()


# ------------------------------------
# plot daily PCC for highly sensitive paramters only (model output = Ave.Conc.benth)
# ------------------------------------

cont1<- pcc_day_ben_conc%>%select(one_of(c("date2","CN_c", "kd","benthic_depth","anae_aq")))

melted_pwc1 = melt(cont1, id.vars="date2")
#melted_pwc<- na.omit(melted_pwc)


# save figure as png
png(filename= paste(pwcdir, "figures/pcc_sensitive_ts_pwc_daily_benthic.png", sep=""),width=10, height=10, units="in",res=250) 


# selected days 1097-1897 based on available observed data -- change according to Yuan data 
daily_pcc_ben_conc_high <- ggplot(melted_pwc1, aes(x=date2, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  #scale_x_continuous(limits = c(1097, 1827))+ 
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "Daily PCC for Sensitive Parameters with Bifenthrin Concentration in Benthic Zone (2009 - 2014)", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(daily_pcc_ben_conc_high)
dev.off()




# If you want to compare with your precipitation data and observed data (figure S15)
# ------------------------------------
# precipitation plots
# ------------------------------------
# read in weather file
precip <- read.table(file=paste(pwcdir_weather, "17719_grid_roseville.wea", sep=""), header=FALSE, sep=",")

colnames(precip) <- c("month", "day", "year", "precip_cm", "et_cm", "temp_c", "windspeed_cms", "solar_la")
precip$date <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")

# 2008 - 2014
# save figure as png
png(filename= paste(pwcdir, "figures/precip_08-14.png", sep=""),width=10, height=10, units="in",res=250) 

p_0814 <- ggplot(precip, aes(x=date,y=precip_cm))+
  geom_line()+
  theme_bw()+labs(title = "", x = "", y = "Precipitation (cm)", color = "")+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))
print(p_0814)
dev.off()


# 2009 - 2014
precip_0914 <- precip[c(367:2557),]

# save figure as png
png(filename= paste(pwcdir, "figures/precip_09-14.png", sep=""),width=10, height=10, units="in",res=250) 

p_0914 <- ggplot(precip_0914, aes(x=date,y=precip_cm))+
  geom_line()+
  theme_bw()+labs(title = "", x = "", y = "Precipitation (cm)", color = "")+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))
print(p_0914)
dev.off()




# # ------------------------------------
# # plot daily PCC (for model output = Peak.Conc.H20)  
# # ------------------------------------
# 
# pcc_day_peak_conc <- as.data.frame(cbind(date, tarray_pcc_peak_daily[367:ndays,1:dim(tarray_pcc_peak_daily)[2]]))
# colnames(pcc_day_peak_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
#                                 "bd1","fc","WP","OC","dep","app_rate","app_eff",
#                                 "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
#                                 "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")
# 
# cont <- pcc_day_peak_conc%>% dplyr::select(one_of(c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
#                                                    "bd1","fc","WP","OC","dep","app_rate","app_eff",
#                                                    "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
#                                                    "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))
# 
# melted_pwc = melt(cont, id.vars="date")
# #melted_pwc <- na.omit(melted_pwc)
# 
# 
# # PCC Time Series plots (all variables)
# daily_pcc_peak_conc <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
#   geom_line() +
#   facet_wrap(~variable, scale="free")+
#   guides(fill=FALSE) +  
#   xlab("Simulation Day") + 
#   ylab("Partial Correlation Coefficient") +
#   #annotate("text", x = 1000, y = 0.92, label = "Peak.Conc.H20", size=6) +
#   theme_bw() +
#   scale_x_discrete(breaks = c(61,426,610,791,1035)) +
#   scale_y_continuous(limits=c(min(melted_pwc$value), max(melted_pwc$value))) +
#   theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# 
# 
# 
# # ------------------------------------
# # plot daily PCC for highly sensitive paramters only (model output = Peak.Conc.H20)
# # ------------------------------------
# 
# cont1<- pcc_day_peak_conc%>%select(one_of(c("date","CN_c","kd","FROC2","SUSED","FROC1")))
# 
# melted_pwc1 = melt(cont1, id.vars="date")
# #melted_pwc<- na.omit(melted_pwc)
# 
# # save figure as png
# png(filename= paste(pwcdir, "figures/pcc_sensitive_ts_pwc_daily_peak_h2.png", sep=""),width=10, height=10, units="in",res=250) 
# 
# 
# # selected days 1097-1897 based on available observed data -- change according to Yuan data 
# daily_pcc_peak_conc_high <- ggplot(melted_pwc1, aes(x=date, y=value, group=variable)) +
#   geom_line(aes(colour=melted_pwc1$variable),size=1)+
#   scale_x_continuous(limits = c(1097, 1827))+ 
#   scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
#   theme_bw()+
#   labs(title = "", x = "", y = "PCC", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
#   theme(legend.position = "bottom")
# 
# print(daily_pcc_peak_conc_high)
# dev.off()


# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------