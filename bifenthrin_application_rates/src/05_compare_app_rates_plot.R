# ----------------------------------------------------------------------------
# compare FOL and PGC application rates
# ----------------------------------------------------------------------------

# read FOL data
fol <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_with_ma_folsom.txt",
                       header=F, sep= ",")
fol$date <- seq(as.Date("2009-01-01"), as.Date("2014-12-31"), by="day")#format 1961-01-01
apps <- fol[,c("V6", "date")]
names(apps) <- c("fol", "date")

# read PGC data
pgc <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_with_ma_pgc.txt",
                  header=F, sep= ",")
apps$pgc <- pgc$V6




# plot
color_fol <- "#034e7b"
color_pgc <- "#feb24c"



png(filename= paste("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/figures/compare_app_rates.png"),width=20, height=10, units="in",res=300) 
compare_apps <- ggplot() +
  theme_bw() +
  
  geom_line(aes(y = apps$fol, x = apps$date, colour = "Folsom"),linetype="solid", size=1)+
  geom_line(aes(y = apps$pgc, x = apps$date, colour = "Pleasant Grove Creek"),linetype="solid", size=1)+
  
  labs(title = "", x = "", y = "Daily Bifenthrin Application (kg/ha)", color = "") +
  
  theme(legend.position = "bottom") +
  
  scale_x_date(date_breaks="1 year", date_labels="%Y", limits=as.Date(c('2011-01-01', '2014-12-31')), expand=c(0.01,0.01)) +
  scale_color_manual(name="", values=c("Folsom"=color_fol, "Pleasant Grove Creek"=color_pgc))+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.y = element_text(size = 12))+ #y axis text size
  theme(axis.text.x = element_text(size = 12))+ #x axis text size
  theme(axis.title.y = element_text(size = 12))+ #y axis title size
  theme(legend.text = element_text(size=12))

print(compare_apps)
dev.off()



