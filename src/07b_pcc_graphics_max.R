# ------------------------------------------------------------------------------
# Graphics of max PCC concentrations
# ------------------------------------------------------------------------------

# SSUMATHY'S CODE ... STILL NEED TO EDIT

#load data
sens_pwc_h2 <- read.csv(file = paste(pwcdir,"io/pcc_pwc_max_h2.csv", sep=""), header=TRUE, sep=",")

#attach file. It will be easy later define your variables
#attach(sens_pwc_h2)
#import ggplot2 library
library(ggplot2)

#Order parameters from high to low
sens_pwc_h2$var <- factor(sens_pwc_h2$var, levels=unique(as.character(sens_pwc_h2$var)) )
#The second orders the levels based on another variable (value in this case):

#save figure as png
#png(filename="E:/office harddrive/Urban_Pesticide_Modeling/PWC/MCREsults/sensitivity.png",width=10, height=10, units="in",res=250) 

sens_pwc_h2_trans<- transform(sens_pwc_h2, variable=reorder(sens_pwc_h2$var, abs(sens_pwc_h2$pcc))) 



p <-ggplot(data=sens_pwc_h2, aes(sens_pwc_h2$pcc,sens_pwc_h2$var))+
  geom_segment(aes(x=0, xend=pcc,y=var,yend=var))#+geom_point()#

p +  facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(pcc), c(-Inf, 0.3, 0.5, Inf))),size = 6) +
  scale_color_manual(name = "|PCC|", values = c("(-Inf,0.3]" = "deepskyblue",
                                                "(0.3,0.5]" = "dodgerblue4",
                                                "(0.5, Inf]" = "Red"), labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
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
  theme(legend.position='bottom')+#add "none" to remove legend
  ggtitle("Parameters vs Maximum Concentration")

#dev.off()







# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------