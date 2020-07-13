# ------------------------------------------------------------------------------
# Graphics of max PCC concentrations
# ------------------------------------------------------------------------------


## EMMA - these PCC plots are displaying: 
##    Parameters vs. Maximum Daily Average Bifenthrin Concentration in Water/Runoff/Sediment


# abs(pcc) cutoff value
cutoff <- 0.05 #re-assess after re-running pwc

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
  scale_color_manual(name = "|PCC|",
                     values = c("(-Inf,0.3]" = "deepskyblue",
                                "(0.3,0.5]" = "dodgerblue4",
                                "(0.5, Inf]" = "Red"),
                     labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(sens_pwc_h2$var))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=14,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=12))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='bottom')+#add "none" to remove legend
  ggtitle("Bifenthrin Concentration in Water Column")+ #Ave.Conc.H20
  scale_y_discrete(labels=c("bulk_density" = "bulk density", "PLMAS"="plmas", "sused"="sused",
                            "FROC1"="froc1", "DOC1"="doc1", "FROC2" = "froc2", "uslek"="uslek",
                            "benthic_depth"="benthic depth", "kd" = "kd","uslep" = "uslep",
                            "app_rate"="app rate", "uslels"="uslels", "uslec_c"="uslec c", 
                            "CN_c"="curve number", "SUSED"="sused"))
print(p1)
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
  scale_color_manual(name = "|PCC|  ",
                     values = c("(-Inf,0.3]" = "deepskyblue",
                                #"(0.3,0.5]" = "dodgerblue4",
                                "(0.5, Inf]" = "Red"),
                     labels = c("> 0.5"))+ # "0.3 \u2264 0.5",
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=14,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y="")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='bottom')+#add "none" to remove legend
  ggtitle("Bifenthrin Concentration in Runoff")+ #RUNF0
  scale_y_discrete(labels=c("CN_c" = "curve number"))

print(p3)
dev.off()




# ------------------------------------------------------------------------------
# Output Variable == Max(Ave.Conc.benthic * 1000000 * Conversion Factor) == Sed Conc.
# ------------------------------------------------------------------------------

# load data
sens_pwc_ben <- read.csv(file = paste(pwcdir,"io/pcc_max_sed.csv", sep=""), header=TRUE, sep=",")

# arrange according to absolute value
sens_pwc_ben <- arrange(sens_pwc_ben, abs(pcc))

# factor var
sens_pwc_ben$var <- factor(sens_pwc_ben$var, levels=unique(as.character(sens_pwc_ben$var)))

# subset variables with largest abs(pcc)
sens_pwc_ben <- sens_pwc_ben %>%
  filter(abs(pcc) > cutoff)

# save figure as png
png(filename= paste(pwcdir, "figures/pcc_pwc_max_sed.png", sep=""),width=10, height=10, units="in",res=250) 

# plot
p4 <-ggplot(data=sens_pwc_ben, aes(sens_pwc_ben$pcc,sens_pwc_ben$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) + 
  #facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|",
                     values = c("(-Inf,0.3]" = "deepskyblue",
                                "(0.3,0.5]" = "dodgerblue4",
                                "(0.5, Inf]" = "Red"),
                     labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=14,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=14,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "")+
  theme(legend.title =element_text(size=12))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='bottom')+#add "none" to remove legend
  ggtitle("Bifenthrin Concentration in Sediment")+
  scale_y_discrete(labels=c("bulk_density"="bulk density","anae_aq"="anae_aq", 
                            "uslek"="uslek", "uslep"="uslep","app_rate"="app rate",
                            "uslels"="uslels", "uslec_c"="uslec c",
                            "benthic_depth"="benthic depth", "CN_c"="curve number",
                            "anae_aq"="anae aq"))
print(p4)
dev.off()


# -----------------------
# dummy plot
# -----------------------

sens_dummy <- sens_pwc_ben[1:3,]
sens_dummy$pcc <- c(0.7, 0.4, 0.2)

# plot
pdummy <-ggplot(data=sens_dummy, aes(sens_dummy$pcc,sens_dummy$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var)) + 
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|",
                     values = c("(-Inf,0.3]" = "deepskyblue",
                                "(0.3,0.5]" = "dodgerblue4",
                                "(0.5, Inf]" = "Red"),
                     labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  theme_bw()+ 
  geom_vline(aes(xintercept=0))+
  labs(fill = "P value", size="Sensitivity Slope") +
  theme(strip.text=element_text(color="Black", size=14,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=14,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+
  labs(x = "PCC", y = "")+
  theme(legend.title =element_text(size=12))+ 
  theme(legend.position='bottom')#add "none" to remove legend
print(pdummy)
dev.off()


# ------------------------------------------------------------------------------
# arrange plots in a panel
# ------------------------------------------------------------------------------

# plot everything together
grid.newpage()

# plot the 3 side-by-side
panel_plot <- cowplot::plot_grid(
  p1 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  align = 'vh',hjust = -1,nrow = 1)

# extract a legend that is laid out horizontally
legend_b <- cowplot::get_legend(
  pdummy + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).

png(filename= paste(pwcdir, "figures/pcc_h2_runf_sed_panel.png", sep=""),width=20, height=10, units="in",res=300) 
final_plot <- cowplot::plot_grid(panel_plot, legend_b, ncol = 1, rel_heights = c(1, .1))
print(final_plot)
dev.off()



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------