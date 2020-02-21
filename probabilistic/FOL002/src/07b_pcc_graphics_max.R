# ------------------------------------------------------------------------------
# Graphics of max PCC concentrations
# ------------------------------------------------------------------------------

# abs(pcc) cutoff value
cutoff <- 0.03 #re-assess after re-running pwc

# ------------------------------------------------------------------------------
# Output Variable == Max(Ave.Conc.H20 * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_h2 <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_h2.csv", sep=""), header=TRUE, sep=",")

# arrange according to absolute value
sens_pwc_h2 <- arrange(sens_pwc_h2, abs(pcc))

# factor var
sens_pwc_h2$var <- factor(sens_pwc_h2$var, levels=unique(as.character(sens_pwc_h2$var)))

# subset variables with largest abs(pcc)
sens_pwc_h2 <- sens_pwc_h2 %>%
  filter(abs(pcc) > cutoff)


# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_h2.png", sep=""),width=10, height=10, units="in",res=250) 

# plot
p1 <- ggplot(data=sens_pwc_h2, aes(x= sens_pwc_h2$pcc, y=sens_pwc_h2$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) +
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "#6baed6",
                                                "(0.3,0.5]" = "#08519c",
                                                "(0.5, Inf]" = "#f03b20"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(sens_pwc_h2$var))))+#scale_x_reverse()+
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
  ggtitle("Parameters vs Maximum Daily Aqueous Average Bifenthrin Concentration in Water Column") #Ave.Conc.H20
print(p1)
dev.off()



# ------------------------------------------------------------------------------
# Output Variable == Max(Peak.Conc.H20 * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_peak <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_peak.csv", sep=""), header=TRUE, sep=",")

# arrange according to absolute value
sens_pwc_peak <- arrange(sens_pwc_peak, abs(pcc))

# factor var
sens_pwc_peak$var <- factor(sens_pwc_peak$var, levels=unique(as.character(sens_pwc_peak$var)))

# subset variables with largest abs(pcc)
sens_pwc_peak <- sens_pwc_peak %>%
  filter(abs(pcc) > cutoff)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_peak.png", sep=""),width=10, height=10, units="in",res=250) 

# plot
p2 <-ggplot(data=sens_pwc_peak, aes(sens_pwc_peak$pcc,sens_pwc_peak$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) +
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
  ggtitle("Parameters vs Maximum Daily Aqueous Peak Bifenthrin Concentration in Water Column") #Peak.Conc.H20

print(p2)
dev.off()




# ------------------------------------------------------------------------------
# Output Variable == Max(PRZM RUNF0)
# ------------------------------------------------------------------------------

# load data
sens_przm_h2 <- read.csv(file = paste(pwcdir,"io/pcc_przm_max_h2.csv", sep=""), header=TRUE, sep=",")

# arrange according to absolute value
sens_przm_h2 <- arrange(sens_przm_h2, abs(pcc))

# factor var
sens_przm_h2$var <- factor(sens_przm_h2$var, levels=unique(as.character(sens_przm_h2$var)))

# subset variables with largest abs(pcc)
sens_przm_h2 <- sens_przm_h2 %>%
  filter(abs(pcc) > cutoff)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_przm_max_h2.png", sep=""),width=10, height=10, units="in",res=250) 

# plot
p3 <-ggplot(data=sens_przm_h2, aes(sens_przm_h2$pcc,sens_przm_h2$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) +
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

print(p3)
dev.off()




# ------------------------------------------------------------------------------
# Output Variable == Max(Ave.Conc.benthic * 1000000)
# ------------------------------------------------------------------------------

# load data
sens_pwc_ben <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_benthic.csv", sep=""), header=TRUE, sep=",")

# arrange according to absolute value
sens_pwc_ben <- arrange(sens_pwc_ben, abs(pcc))

# factor var
sens_pwc_ben$var <- factor(sens_pwc_ben$var, levels=unique(as.character(sens_pwc_ben$var)))

# subset variables with largest abs(pcc)
sens_pwc_ben <- sens_pwc_ben %>%
  filter(abs(pcc) > cutoff)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_benthic.png", sep=""),width=10, height=10, units="in",res=250) 

# plot
p4 <-ggplot(data=sens_pwc_ben, aes(sens_pwc_ben$pcc,sens_pwc_ben$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) + 
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
  ggtitle("Parameters vs Maximum Daily Aqueous Average Bifenthrin Concentration in Benthic Zone") #Ave.Conc.benthic
print(p4)
dev.off()






# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------