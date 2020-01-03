# ------------------------------------------------------------------------------
# Graphics of daily PCC concentrations
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------
# load data, call variables
# -----------------------------------------------------------------------

ndays <- length(timearray)

date <- 34:ndays # might need to change


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

pcc_day_ave_conc <- as.data.frame(cbind(date, tarray_pcc_ave_conc_daily[34:ndays,1:dim(tarray_pcc_ave_conc_daily)[2]]))
colnames(pcc_day_ave_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                       "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                       "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                       "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

cont <- pcc_day_ave_conc%>% dplyr::select(one_of(c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                         "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                                         "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                         "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date")
#melted_pwc <- na.omit(melted_pwc)


# PCC Time Series plots (all variables)
daily_pcc_ave_conc <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc$variable)) +
  facet_wrap(~variable, scale="free")+
  guides(fill=FALSE) +  
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  #annotate("text", x = 1000, y = 0.92, label = "Ave.Conc.H20", size=6) +
  theme_bw() +
  scale_x_discrete(breaks = c(61,426,610,791,1035)) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())




# ------------------------------------
# plot daily PCC for highly sensitive parameters only (model output = Ave.Conc.H20)
# ------------------------------------

cont1<- pcc_day_ave_conc%>%select(one_of(c("date","CN_c","uslec_c","dep","app_rate","app_eff","benthic_depth")))

melted_pwc1 = melt(cont1, id.vars="date")
#melted_pwc<- na.omit(melted_pwc)

# selected days 1097-1897 based on available observed data -- change according to Yuan data 
daily_pcc_ave_conc_high <- ggplot(melted_pwc1, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_x_continuous(limits = c(1097, 1827))+ 
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")






# ------------------------------------
# plot daily PCC (for model output = Peak.Conc.H20)  
# ------------------------------------

pcc_day_peak_conc <- as.data.frame(cbind(date, tarray_pcc_peak_daily[34:ndays,1:dim(tarray_pcc_peak_daily)[2]]))
colnames(pcc_day_peak_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                                "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

cont <- pcc_day_peak_conc%>% dplyr::select(one_of(c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                                   "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                                                   "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                                   "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date")
#melted_pwc <- na.omit(melted_pwc)


# PCC Time Series plots (all variables)
daily_pcc_peak_conc <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc$variable)) +
  facet_wrap(~variable, scale="free")+
  guides(fill=FALSE) +  
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  #annotate("text", x = 1000, y = 0.92, label = "Peak.Conc.H20", size=6) +
  theme_bw() +
  scale_x_discrete(breaks = c(61,426,610,791,1035)) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())




# ------------------------------------
# plot daily PCC for highly sensitive paramters only (model output = Peak.Conc.H20)
# ------------------------------------

cont1<- pcc_day_peak_conc%>%select(one_of(c("date","CN_c","uslec_c","dep","app_rate","app_eff","anae_aq", "temp_ref_anae","bulk_density")))

melted_pwc1 = melt(cont1, id.vars="date")
#melted_pwc<- na.omit(melted_pwc)

# selected days 1097-1897 based on available observed data -- change according to Yuan data 
daily_pcc_peak_conc_high <- ggplot(melted_pwc1, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_x_continuous(limits = c(1097, 1827))+ 
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")










# ------------------------------------
# plot daily PCC (for model output = Ave.Conc.benth)  
# ------------------------------------

pcc_day_ben_conc <- as.data.frame(cbind(date, tarray_pcc_benthic_daily[34:ndays,1:dim(tarray_pcc_benthic_daily)[2]]))
colnames(pcc_day_ben_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                 "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                                 "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                 "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

cont <- pcc_day_ben_conc%>% dplyr::select(one_of(c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                                    "depth","COVMAX","HTMAX","holdup","bd1","fc","WP","OC","dep","app_rate","app_eff",
                                                    "DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                                    "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date")
#melted_pwc <- na.omit(melted_pwc)


# PCC Time Series plots (all variables)
daily_pcc_ben_conc <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc$variable)) +
  facet_wrap(~variable, scale="free")+
  guides(fill=FALSE) +  
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  #annotate("text", x = 1000, y = 0.92, label = "Ave.Conc.Benthic", size=6) +
  theme_bw() +
  scale_x_discrete(breaks = c(61,426,610,791,1035)) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())




# ------------------------------------
# plot daily PCC for highly sensitive paramters only (model output = Ave.Conc.benth)
# ------------------------------------

cont1<- pcc_day_ben_conc%>%select(one_of(c("date","CN_c","uslec_c","dep","app_rate","app_eff","anae_aq","temp_ref_ae")))

melted_pwc1 = melt(cont1, id.vars="date")
#melted_pwc<- na.omit(melted_pwc)

# selected days 1097-1897 based on available observed data -- change according to Yuan data 
daily_pcc_ben_conc_high <- ggplot(melted_pwc1, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_x_continuous(limits = c(1097, 1827))+ 
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")






# # If you want to compare with your precipitation data and observed data (figure S15)
# # ------------------------------------
# # precipitation plot
# # ------------------------------------
# prec<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/watershed1/precip.csv", header=TRUE, 
#                 sep=",")
# prec$Date <- as.Date(prec$Date, "%m/%d/%Y")
# 
# #prec23<-subset(prec, prec$Date> 1/1/2002 & prec$Date < 1/1/2003)
# prec23<-prec %>%filter(Date >"2002-01-01", Date <"2004-01-01") #comma same as &
# 
# P <-ggplot(prec23, aes(x=Date,y=PRCP0))+geom_line()+
#   theme_bw()+labs(title = "", x = "", y = "Precipitation (cm)", color = "")+ 
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
#   theme(legend.text=element_text(size=12))+
#   theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))
# 





# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------