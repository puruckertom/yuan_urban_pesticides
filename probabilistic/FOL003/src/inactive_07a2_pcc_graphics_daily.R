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



# -----------------------------------------------------------------------
# plots   (for model output = Ave.Conc.H20)
# -----------------------------------------------------------------------

pcc_day_ave_conc <- as.data.frame(cbind(date, tarray_pcc_ave_conc_daily[367:ndays,1:dim(tarray_pcc_ave_conc_daily)[2]]))
colnames(pcc_day_ave_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

pcc_day_ave_conc$date2 <-seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="days")#format 1961-01-01

cont <- pcc_day_ave_conc%>% dplyr::select(one_of(c("date2","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                                   "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                                   "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))


# sensitive parameters only 
cont1<- pcc_day_ave_conc%>%select(one_of(c("date2","CN_c","bulk_density", "kd", "FROC2")))

melted_pwc1 = melt(cont1, id.vars="date2")
melted_pwc1<- na.omit(melted_pwc1)

daily_pcc_ave_conc_high <- ggplot(melted_pwc1, aes(x=date2, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  scale_x_date(date_breaks="6 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  theme_bw()+
  labs(title = "Daily PCC for Sensitive Parameters with Bifenthrin Concentration in Water Column (2009 - 2014)", x = "Day", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(daily_pcc_ave_conc_high)


# plot together
png(filename= paste(pwcdir, "figures/pcc_sensitive_rainfall_09-14_pwc_daily_ave_h2.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p_0914)
g3 <- ggplotGrob(daily_pcc_ave_conc_high)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()





# -----------------------------------------------------------------------
# plot daily PCC (for model output = Ave.Conc.benth)
# -----------------------------------------------------------------------
pcc_day_ben_conc <- as.data.frame(cbind(date, tarray_pcc_benthic_daily[367:ndays,1:dim(tarray_pcc_benthic_daily)[2]]))
colnames(pcc_day_ben_conc) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

pcc_day_ben_conc$date2 <-seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="days")#format 1961-01-01

cont <- pcc_day_ben_conc%>% dplyr::select(one_of(c("date2","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","uslec_c","MNGN",
                                                   "bd1","fc","WP","OC","app_rate","DWRATE","DSRATE","kd","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                                   "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))


# sensitive parameters only 
cont1<- pcc_day_ben_conc%>%select(one_of(c("date2","kd","benthic_depth","anae_aq", "CN_c")))

melted_pwc1 = melt(cont1, id.vars="date2")
#melted_pwc<- na.omit(melted_pwc)

daily_pcc_ben_conc_high <- ggplot(melted_pwc1, aes(x=date2, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+
  scale_x_date(date_breaks="6 months", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+
  labs(title = "Daily PCC for Sensitive Parameters with Bifenthrin Concentration in Benthic Zone (2009 - 2014)", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "bottom")
print(daily_pcc_ben_conc_high)


# plot together
png(filename= paste(pwcdir, "figures/pcc_sensitive_rainfall_09-14_pwc_daily_benthic.png", sep=""),width=20, height=10, units="in",res=300) 

g2 <- ggplotGrob(p_0914)
g3 <- ggplotGrob(daily_pcc_ben_conc_high)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

plot_output <- grid.draw(g)

print(plot_output)
dev.off()


# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------