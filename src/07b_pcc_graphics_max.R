# ------------------------------------------------------------------------------
# Graphics of max PCC concentrations
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Output Variable == Max(Ave.Conc.H20 * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_h2 <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_h2.csv", sep=""), header=TRUE, sep=",")

# attach file. It will be easy later define your variables
#attach(sens_pwc_h2)

# factor var
sens_pwc_h2$var <- factor(sens_pwc_h2$var, levels=unique(as.character(sens_pwc_h2$var)))

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_h2.png", sep=""),width=10, height=10, units="in",res=250) 


# plot
p1 <-ggplot(data=sens_pwc_h2, aes(sens_pwc_h2$pcc,sens_pwc_h2$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var))#+geom_point()#

# plot with details
p1 +  
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "#6baed6",
                                                "(0.3,0.5]" = "#08519c",
                                                "(0.5, Inf]" = "#f03b20"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=12,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='none')+#add "none" to remove legend
  ggtitle("Parameters vs Maximum Daily Aqueous Average Bifenthrin Conc. in Pool Water Column") #Ave.Conc.H20

dev.off()





# ------------------------------------------------------------------------------
# Output Variable == Max(Peak.Conc.H20 * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_peak <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_peak.csv", sep=""), header=TRUE, sep=",")

# attach file. It will be easy later define your variables
#attach(sens_pwc_peak)

# factor var
sens_pwc_peak$var <- factor(sens_pwc_peak$var, levels=unique(as.character(sens_pwc_peak$var)))

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_peak.png", sep=""),width=10, height=10, units="in",res=250) 


# plot
p2 <-ggplot(data=sens_pwc_peak, aes(sens_pwc_peak$pcc,sens_pwc_peak$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var))#+geom_point()#

# plot with details
p2 +  
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "#6baed6",
                                                "(0.3,0.5]" = "#08519c",
                                                "(0.5, Inf]" = "#f03b20"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=12,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='none')+#add "none" to remove legend
  ggtitle("Parameters vs Maximum Daily Aqueous Peak Bifenthrin Conc. in Pool Water Column") #Peak.Conc.H20

dev.off()




# ------------------------------------------------------------------------------
# Output Variable == Max(PRZM RUNF0)
# ------------------------------------------------------------------------------

# load data
sens_przm_h2 <- read.csv(file = paste(pwcdir,"io/pcc_przm_max_h2.csv", sep=""), header=TRUE, sep=",")

# attach file. It will be easy later define your variables
#attach(sens_przm_h2)

# factor var
sens_przm_h2$var <- factor(sens_przm_h2$var, levels=unique(as.character(sens_przm_h2$var)))

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_przm_max_h2.png", sep=""),width=10, height=10, units="in",res=250) 


# plot
p3 <-ggplot(data=sens_przm_h2, aes(sens_przm_h2$pcc,sens_przm_h2$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var))#+geom_point()#

# plot with details
p3 +  
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "#6baed6",
                                                "(0.3,0.5]" = "#08519c",
                                                "(0.5, Inf]" = "#f03b20"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=12,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='none')+#add "none" to remove legend
  ggtitle("Parameters vs Maximum Daily Bifenthrin Concentration in Runoff") #RUNF0

dev.off()




# ------------------------------------------------------------------------------
# Output Variable == Max(Ave.Conc.benthic * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_ben <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_benthic.csv", sep=""), header=TRUE, sep=",")

# attach file. It will be easy later define your variables
#attach(sens_pwc_ben)

# factor var
sens_pwc_ben$var <- factor(sens_pwc_ben$var, levels=unique(as.character(sens_pwc_ben$var)))

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_benthic.png", sep=""),width=10, height=10, units="in",res=250) 


# plot
p4 <-ggplot(data=sens_pwc_ben, aes(sens_pwc_ben$pcc,sens_pwc_ben$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var))#+geom_point()#

# plot with details
p4 +  
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "#6baed6",
                                                "(0.3,0.5]" = "#08519c",
                                                "(0.5, Inf]" = "#f03b20"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=12,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='none')+#add "none" to remove legend
  ggtitle("Parameters vs Maximum Daily Aqueous Average Bifenthrin Conc. in Pool Benthic Column") #Ave.Conc.benthic

dev.off()








# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------