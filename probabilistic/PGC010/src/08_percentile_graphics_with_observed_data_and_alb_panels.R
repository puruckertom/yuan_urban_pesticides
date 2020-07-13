# ------------------------------------------------------------------------------
# percentile graphics with application rates
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# load data
# ------------------------------------------------------------------------------
library(gtable)
Nsims <- 5000

# pwc output array
# recall: this was created in 03write_update_run_pwc and saved in 05_write_output_into_df
# recall: this is an array of all of the output_PGC010_parent_only_Custom_Parent_daily.csv files 
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
percentiles$percent.001 <- percentiles$percent.001*1000000 #convert units to ug/L 
percentiles$percent.023 <- percentiles$percent.023*1000000 
percentiles$percent.159 <- percentiles$percent.159*1000000 
percentiles$percent.5 <- percentiles$percent.5*1000000 
percentiles$percent.841 <- percentiles$percent.841*1000000  
percentiles$percent.977 <- percentiles$percent.977*1000000  
percentiles$percent.999 <- percentiles$percent.999*1000000  




# read in deterministic output
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/PGC010/outputs/output_PGC010_parent_only_Custom_Parent_daily.csv",
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
# read in observed data
# --------------------------------

obs_water <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/observed_concentrations/cdpr_stormdrain_bifenthrin_water_09-14_all.csv",
                      header=T, sep=",")

# change column formats
obs_water$date <- as.Date(obs_water$Sample.Date, format="%d-%b-%Y")
obs_water$Result <- as.numeric(levels(obs_water$Result))[obs_water$Result]

# subset pgc sites
obs_water_pgc <- obs_water[which(obs_water$Site.ID == "PGC010"), ]



# --------------------------------
# plot percentile data + observed data + tox rates
# --------------------------------

# set colors
sd3 <- "#08519c"
sd2 <- "#4292c6"
sd1 <- "#9ecae1"
med <- "#08519c"
det <- "#d9f0a3"
obs <- "#e31a1c"
fish <- "#fee391"
invert <- "#ec7014"


# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  
  geom_hline(aes(yintercept=0.075, color="Acute/Chronic Fish"), linetype="dashed", size=1) + #aquatic life benchmarks for fish and invertebrates
  geom_hline(yintercept=0.04 , linetype="dashed", color=fish, size=1) +
  geom_hline(yintercept=0.8, linetype="dashed", color=invert, size=1) +
  geom_hline(aes(yintercept=0.0013 , color="Acute/Chronic Invertebrate"),linetype="dashed", size=1) +
  
  geom_line(aes(y=percent.5, color="Probabilistic Median"), linetype="solid", size=1) + #probabilistic
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) + #deterministic
  
  geom_point(data=obs_water_pgc, aes(x=date, y=Result, color="CDPR Observed"), size=3)+ # CDPR observed data
  
  scale_x_date(date_breaks="1 year", labels=NULL, limits=as.Date(c('2011-01-01', '2014-12-31')), expand=c(0.01,0.01)) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,20)) +
  labs(title = "", x = "", y = "Bifenthrin Concentration \n in the Water Column (ug/L) (log10)", color = "") +
  theme_bw() +
  #theme(legend.justification=c(0,0), legend.position=c(0.01,0.01), legend.box="horizontal")+
  #guides(colour = guide_legend(nrow = 1))+
  theme(legend.position="bottom")+
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("CDPR Observed"=obs, "Deterministic"=det,"Probabilistic Median" =med,
                                       "Acute/Chronic Invertebrate"=invert, "Acute/Chronic Fish" = fish))+
  theme(axis.text.y = element_text(size = 12))+ #axis text size
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)))+ #y axis label
  theme(plot.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 10)) 

print(pwc_pplot)



# read in app rate data
calpip_s <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_with_ma_pgc.txt",
                     header=F, sep= ",")
calpip_s$date <- seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="day")#format 1961-01-01
calpip_s <- calpip_s[,c("V6", "date")]
names(calpip_s) <- c("app_rate", "date")

a_plot <- ggplot(data=calpip_s, aes(x=date, y=app_rate)) +
  geom_bar(stat="identity", fill="#525252") +
  labs(title = "", x = "", y = "Bifenthrin Application (kg/ha)", color = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_date(date_breaks="1 year", date_labels="%Y", limits=as.Date(c('2011-01-01', '2014-12-31')), expand=c(0.01,0.01)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text(size = 12))+ #axis text size
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))
print(a_plot)

# read in weather file
precip <- read.table(file=paste(pwcdir_weather, "17719_grid_roseville.wea", sep=""), header=FALSE, sep=",")

colnames(precip) <- c("month", "day", "year", "precip_cm", "et_cm", "temp_c", "windspeed_cms", "solar_la")
precip$date <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")

p_plot <- ggplot(precip, aes(x=date,y=precip_cm))+
  geom_bar(stat="identity", fill="black")+
  theme_bw()+
  labs(title = "", x = "", y = "Precipitation (cm)", color = "") +
  scale_y_reverse() +
  scale_x_date(date_breaks="1 year", labels=NULL, limits=as.Date(c('2011-01-01', '2014-12-31')), expand=c(0.01,0.01)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text(size = 12))+ #axis text size
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 25, b = 0, l = 0)))
print(p_plot)


# plot everything together
grid.newpage()

png(filename= paste(pwcdir, "figures/percentile_11-14_pwc_ave_h2_panel.png", sep=""),width=20, height=10, units="in",res=300) 
panel_plot <- cowplot::plot_grid(p_plot, pwc_pplot, a_plot, align = "h", nrow = 3, rel_heights = c(0.25, 0.5, 0.25))
panel_plot <- egg::ggarrange(p_plot, pwc_pplot, a_plot, heights = c(0.25, 0.5, 0.25))
print(panel_plot)
dev.off()

# ------------------------------------------------------------------------------
# percentile plot: pwc Ave.Conc.benth
# ------------------------------------------------------------------------------

# --------------------------------
# data set-up
# --------------------------------

load(paste(pwcdir, "io/con_fac_output.RData", sep = ""))
dim(con_fac_output) #sims*1

dim(pwc_ben_output) #days*sims

# convert to sediment concentrations 
sed_output <- pwc_ben_output
for (c in 1:dim(sed_output)[2]){
  this_con_fac <- con_fac_output[c,]
  for (r in 1:dim(sed_output)[1]){
    sed_output[r,c] <- sed_output[r,c]*this_con_fac
  }
}


# create blank matrix to fill with percentiles
percentiles <- matrix(data=NA, nrow=dim(sed_output)[1], ncol=8)
colnames(percentiles) <- c("day", "percent.001", "percent.023", "percent.159", "percent.5",
                           "percent.841", "percent.977", "percent.999")
percentiles <- as.data.frame(percentiles)

# date format
percentiles$day <- seq(as.Date("2008-01-01"), as.Date("2014-12-31"), by="days")


# compute percentiles
for (i in 1:dim(percentiles)[1]){
  p001 <- quantile(sed_output[i,], probs=.001, na.rm=T)
  percentiles[i,2] <- p001
  
  p023 <- quantile(sed_output[i,], probs=.023, na.rm=T)
  percentiles[i,3] <- p023
  
  p159 <- quantile(sed_output[i,], probs=.159, na.rm=T)
  percentiles[i,4] <- p159
  
  p5 <- quantile(sed_output[i,], probs=.5, na.rm=T)
  percentiles[i,5] <- p5
  
  p841 <- quantile(sed_output[i,], probs=.841, na.rm=T)
  percentiles[i,6] <- p841
  
  p977 <- quantile(sed_output[i,], probs=.977, na.rm=T)
  percentiles[i,7] <- p977
  
  p999 <- quantile(sed_output[i,], probs=.999, na.rm=T)
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
determ <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/PGC010/outputs/output_PGC010_parent_only_Custom_Parent_daily.csv",
                   header= FALSE, sep= ",", skip = 5, stringsAsFactors = FALSE, row.names=NULL)
colnames(determ) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")
determ <- as.data.frame(determ)

# read conversion factor from output 
con <- file("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/PGC010/outputs/output_PGC010_parent_only_Custom_Parent.txt")
open(con)
con_fac_line <- read.table(con,skip=15,nrow=1) #16-th line
con_fac <- as.numeric(con_fac_line%>%select_if(is.numeric))
print(con_fac)
close(con)

# subset Ave.conc.benth, conversions, add to percentiles df
percentiles$deterministic <- determ$Ave.Conc.benth*1000000*con_fac #convert units to ug/L


# impose a false zero
for (i in 1:dim(percentiles)[1]){
  if (percentiles[i,2] < 1e-8){
    percentiles[i,2] <- 1e-8
  } 
} 


# --------------------------------
# read in observed data
# --------------------------------

obs_sed <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/observed_concentrations/cdpr_stormdrain_bifenthrin_sediment_09-14_all.csv",
                    header=T, sep=",")

# subset pgc sites
obs_sed_pgc <- obs_sed[which(obs_sed$Site.ID == "PGC010"), ]

# change column formats
obs_sed_pgc$date <- as.Date(obs_sed_pgc$Sample.Date, format="%d-%b-%y")



# --------------------------------
# plot percentiles
# --------------------------------

# set colors
sd3 <- "#6a51a3"
sd2 <- "#807dba"
sd1 <- "#bcbddc"
med <- "#6a51a3"
det <- "#d9f0a3"
fish <- "#fee391"
invert <- "#ec7014"

# plot
pwc_pplot <- ggplot(percentiles, aes(x=day, group=1)) +
  geom_ribbon(aes(ymin=percent.001, ymax=percent.999, fill="3 SD")) +
  geom_ribbon(aes(ymin=percent.023, ymax=percent.977, fill="2 SD")) +
  geom_ribbon(aes(ymin=percent.159, ymax=percent.841, fill="1 SD")) +
  
  geom_hline(yintercept=200, linetype="dashed", color=invert, size=1) +
  geom_hline(aes(yintercept=0.13 , color="Acute/Chronic Invertebrate"),linetype="dashed", size=1) +
  
  geom_line(aes(y=percent.5, color="Probabilistic Median"), linetype="solid", size=1) +
  geom_line(aes(y=deterministic, color="Deterministic"), linetype="solid", size=1) +
  
  geom_point(data=obs_sed_pgc, aes(x=date, y=Result, color="CDPR Observed"), size=3)+ # CDPR observed data
  
  scale_x_date(date_breaks="1 year", labels=NULL, limits=as.Date(c('2011-01-01', '2014-12-31')), expand=c(0.01,0.01)) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,40000)) +
  labs(title = "", x = "", y = "Bifenthrin Sediment Concentration \n (total mass, ug)/(dry sed mass,kg) (log10)", color = "") +
  theme_bw() +
  #theme(legend.justification=c(0,0), legend.position=c(0.01,0.01), legend.box="horizontal")+
  #guides(colour = guide_legend(nrow = 1))+
  theme(legend.position = "bottom")+
  scale_fill_manual(name="", values=c("3 SD"=sd3, "2 SD"=sd2, "1 SD" =sd1))+
  scale_color_manual(name="", values=c("CDPR Observed"=obs, "Deterministic"=det,"Probabilistic Median" =med,
                                       "Acute/Chronic Invertebrate"=invert))+
  theme(axis.text.y = element_text(size = 12))+ #axis text size
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(plot.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 10))

print(pwc_pplot)
dev.off()


# plot everything together
grid.newpage()

png(filename= paste(pwcdir, "figures/percentile_11-14_pwc_ave_benthic_panel.png", sep=""),width=20, height=10, units="in",res=300) 
panel_plot <- cowplot::plot_grid(p_plot, pwc_pplot,a_plot, align = "h", nrow = 3, rel_heights = c(0.25, 0.5, 0.25))
panel_plot <- egg::ggarrange(p_plot, pwc_pplot,a_plot, heights = c(0.25, 0.5, 0.25))
print(panel_plot)
dev.off()



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------