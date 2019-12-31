# -----------------------------------------------------------------------
# plotting sensitivity analysis -- max
# -----------------------------------------------------------------------
#
#
# --------------------------------------------------------------------------
#load data
sens <- read.csv("E:/office harddrive/Urban_Pesticide_Modeling/PWC/MCREsults/31_29_max_sens.csv", header=TRUE, sep=",")
#attach file. It will be easy later define your variables
attach(sens)
#import ggplot2 library
library(ggplot2)

#Order parameters from high to low
sens$Parameter <- factor(sens$Parameter, levels=unique(as.character(sens$Parameter)) )
#The second orders the levels based on another variable (value in this case):

#save figure as png
#png(filename="E:/office harddrive/Urban_Pesticide_Modeling/PWC/MCREsults/sensitivity.png",width=10, height=10, units="in",res=250) 

sens<- transform(sens, variable=reorder(Parameter, abs(PCC))) 

p <-ggplot(data=sens, aes(PCC,variable))+geom_segment(aes(x=0, xend=PCC,y=variable,yend=variable))#+geom_point()#
p +  facet_grid (~Media,scales="free")+
  geom_point(aes(colour = cut(abs(PCC), c(-Inf, 0.3, 0.5, Inf))),
             size = 6) +
  scale_color_manual(name = "|PCC|",
                     values = c("(-Inf,0.3]" = "deepskyblue",
                                "(0.3,0.5]" = "dodgerblue4",
                                "(0.5, Inf]" = "Red"),
                     labels = c("\u2264 0.3", "0.3\u2264 0.5", ">0.5"))+
  geom_point(shape=21,size=6)+  
  #scale_y_discrete(limits = rev(unique(sort(trends$LUf))))+#scale_x_reverse()+
  theme_bw()+ geom_vline(aes(xintercept=0))+labs(fill = "P value", size="Sens Slope") +
  theme(strip.text=element_text(color="Black", size=12,face="bold"))+ 
  theme(axis.title.x = element_text( colour="black", size=12,face="bold"),axis.text.x  = element_text(vjust=0.5, size=12,colour="black"))+ 
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, size=12,colour="black"))+
  theme(legend.text=element_text(size=12))+labs(x = "PCC", y = "Parameters")+
  theme(legend.title =element_text(size=14))+ 
  # geom_text(data=subset(sens, PCC > 0.4),
  #           aes(PCC,Parameter,label=Parameter))+
  theme(legend.position='bottom')+#add "none" to remove legend
  ggtitle("Parameters vs maximum concentration")

#dev.off()



# -----------------------------------------------------------------------
# the end
# -----------------------------------------------------------------------