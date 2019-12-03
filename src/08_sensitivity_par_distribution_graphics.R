# -----------------------------------------------------------------------
# sensitivity parameter distribution graphs
# -----------------------------------------------------------------------



# -----------------------------------------------------------------------
# load data
# -----------------------------------------------------------------------

# multiplot function
source(paste(pwcdir, "src/function_multiplot.R", sep = ""))


# ??
par_distribution<- read.csv("C:/git/sinnathamby_pwc/figures/par_distribution.csv", header=TRUE, 
                sep=",")
par_distribution_m <- melt(par_distribution,id.var=1)


# ??
max_distribution<- read.csv("C:/git/sinnathamby_pwc/io/inputdata_przm_vvwm_max.csv", header=TRUE, 
                            sep=",")


# ??
par_distribution_sen<- read.csv("C:/git/sinnathamby_pwc/figures/par_distribution_sen.csv", header=TRUE, 
                                sep=",")
par_distribution_sen_m <- melt(par_distribution_sen,id.var=1)


# ??
par_distribution_sed<- read.csv("C:/git/sinnathamby_pwc/figures/par_distribution_sed.csv", header=TRUE, 
                                sep=",")
par_distribution_sed_m <- melt(par_distribution_sed,id.var=1)


# ??
sim_dia<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sim_dia.csv", header=TRUE, 
                   sep=",")


# ??
sim_chlor<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/chlorpyrifos/sim_chlor.csv", header=TRUE, 
                     sep=",")

# ??
sim_mal<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/Malathion/sim_mal.csv", header=TRUE, 
                   sep=",")


# pwc data
load(paste(pwcdir, "io/pwcout.RData", sep = ""))
head(pwcoutdf)



# -----------------------------------------------------------------------
# plot
# -----------------------------------------------------------------------

# ??
ggplot(par_distribution_m, aes(x=value, y=max_h20))+geom_point(alpha = 0.2)+
  facet_wrap( ~ variable, scales = "free",ncol=8)+theme_bw()+ stat_smooth(method = "loess") 

# ??
ggplot(par_distribution_m, aes(x=value, y=max_h20))+geom_point()+
  facet_wrap( ~ variable, scales = "free",ncol=8)+theme_bw()

# ??
ggplot(par_distribution_sen_m, aes(x=value, y=max_h20))+geom_point()+
  facet_wrap( ~ variable, scales = "free",ncol=3)+theme_bw() 

# ??
ggplot(par_distribution_sed_m, aes(x=value, y=max_sed))+geom_point()+
  facet_wrap( ~ variable, scales = "free",ncol=3)+theme_bw()


# ????
a<-ggplot()+ 
  geom_histogram(aes(max_distribution$pwc_max_h2),fill = "red", alpha = 0.2,color="black")+scale_x_log10()

b<-ggplot()+ 
  geom_histogram(aes(max_distribution$max_sed),fill = "green", alpha = 0.2,color="black")+scale_x_log10()

c<-ggplot()+
  geom_histogram(aes(max_distribution$pwc_max_benthic),fill = "blue", alpha = 0.2,color="black")+scale_x_log10()

d<-a+
  geom_vline(aes(xintercept=median(max_distribution$pwc_max_h2)),color="red", linetype="dashed", size=1)+
  theme_classic()+
  labs(x = "Maximum Pesticide Concentration in Water") 

e<-b+
  geom_vline(aes(xintercept=median(max_distribution$max_sed)), color="green", linetype="dashed", size=1)+
  theme_classic()+
  labs(x = "Maximum Pesticide Concentration in Sediment") 

f<-c+ 
  geom_vline(aes(xintercept=median(max_distribution$pwc_max_benthic)), color="blue", linetype="dashed", size=1)+
  theme_classic()+
  labs(x = "Maximum Pesticide Concentration in Benthic")


# plot everything together
multiplot(d, e, f, cols=2)


# ???
ggplot() + 
  geom_histogram(aes(max_distribution$pwc_max_h2),fill = "red", alpha = 0.2,color="black")+ 
  geom_histogram(aes(max_distribution$max_sedd),fill = "green", alpha = 0.2,color="black")+
  geom_histogram(aes(max_distribution$max_ben),fill = "blue", alpha = 0.2,color="black")+
  scale_x_log10()



# -----------------------------------------------------------------------
# plot  - different stuff ??
# -----------------------------------------------------------------------


a<-pwcoutdf[,-1,1:Nsims] #no depth 
head(a)

b<-a[,-3,1:Nsims] #no benthic
c<- melt(b)
head(c)
colnames(c) <- c("day","variable","aaa","value")


options("scipen"=100, "digits"=4)

# -----------------------------
# plot  - ??
# -----------------------------
g <- ggplot(c,aes(x=value*1000000))
g <- g + geom_histogram()
g <- g + facet_wrap(~variable)+
  scale_x_log10(limits=c(0.000001, 150))+
  geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.1, color="blue" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=1.1, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.259, color="green", linetype="dashed", size=1)+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+guides(fill=guide_legend(nrow=2,byrow=TRUE)) 


# -----------------------------
# plot  - ??
# -----------------------------
group.colors <- c(rep("red"), rep("blue"))
g <- ggplot(c,aes(x=value*1000000,fill=variable ))
g <- g + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),alpha=0.5,color="black", position="identity")+
  scale_x_log10(limits=c(0.00000001, 100))
g <- g + 
  scale_fill_manual(values=group.colors)+
  theme_classic()+
  #geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.035, color="red" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=0.028, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.17, color="black", size=1)+
  geom_vline(xintercept=0.3, color="red", size=1)+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = "log [Malathion Daily Average Concentration (ug/l)]",y = "Relative Frequency") 


# -----------------------------
# plot  - ??
# -----------------------------
group.colors <- c(rep("red"), rep("blue"))
g <- ggplot(sim_dia,aes(x=sim_dia$con,fill=sim_dia$media ))
g <- g + geom_histogram(aes(y = (..count..)/sum(..count..)),alpha=0.5,color="black", position="identity")+scale_x_log10(limits=c(0.00000001, 100))
g <- g + scale_fill_manual(values=group.colors)+theme_classic()+
  #geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.16, color="red" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=0.10, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.105, color="black", size=1)+
  geom_vline(xintercept=0.105, color="black", size=1)+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = "log [Diazinon Daily Average Concentration (ug/l)]",y = "Relative Frequency") 


# -----------------------------
# plot with all 3 CDF plots
# -----------------------------

# set colors for the CDF
sim_diaCDFcolor <- rgb(1,0,0)
bCDFcolor <- rgb(0,1,0)
cCDFcolor <- rgb(0,0,1)

# create a single chart 
plot(ecdf(sim_dia$con), col=sim_diaCDFcolor, main=NA)
plot(ecdf(b), col=bCDFcolor, add=T)
plot(ecdf(c), col=cCDFcolor, add=T)

# add a legend to the chart
legend('right', c('a', 'b', 'c'), fill=c(sim_diaCDFcolor, bCDFcolor, cCDFcolor), border=NA)




# -----------------------------
# plot  - ??
# -----------------------------
group.colors <- c(rep("red"), rep("blue"))
g <- ggplot(sim_chlor,aes(x=sim_chlor$con,fill=sim_chlor$media ))
g <- g + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),alpha=0.5,color="black", position="identity")+
  scale_x_log10(limits=c(0.00000001, 100))
g <- g + 
  scale_fill_manual(values=group.colors)+
  theme_classic()+
  geom_vline(xintercept=0.04, color="red" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=0.015, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.025, color="black", size=1)+
  geom_vline(xintercept=0.05, color="red", size=1)+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = "log [Chlorpyrifos Daily Average Concentration (ug/l)]",y = "Relative Frequency") 


# -----------------------------
# plot  - ??
# -----------------------------
group.colors <- c(rep("red"), rep("blue"))
g <- ggplot(sim_mal,aes(x=sim_mal$con,fill=sim_mal$media ))
g <- g + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),alpha=0.5,color="black", position="identity")+
  scale_x_log10(limits=c(0.00000001, 100))
g <- g + 
  scale_fill_manual(values=group.colors)+
  theme_classic()+
  geom_vline(xintercept=0.035, color="red" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=0.028, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.17, color="black", size=1)+
  geom_vline(xintercept=0.3, color="red", size=1)+
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
  theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = "log [Malathion Daily Average Concentration (ug/l)]",y = "Relative Frequency") 




# -----------------------------------------------------------------------
# somethin' else goin' on here
# -----------------------------------------------------------------------


sum(c$value*1000000>0.17)
dim(c)
ggplot() + 
  geom_histogram(aes((pwcoutdf["Ave.Conc.H20"])*1000000),fill = "red", alpha = 0.2,color="black")+
  geom_histogram(aes((pwcoutdf["Ave.Conc.benth"])*1000000),fill = "blue", alpha = 0.2,color="black")+
  scale_x_log10()




options("scipen"=100, "digits"=4)
max_con<- read.csv("C:/git/sinnathamby_pwc/io/media_max_con1.csv", header=TRUE, 
                            sep=",")
ggplot(max_con, aes(x=Concentration, color=Media,fill=Media)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),alpha=0.5, position="identity")+
  scale_x_log10()+scale_color_manual(labels = c("Benthic", "Water","Observed","stream"),values=c("green","blue" , "red","black"))+
  scale_fill_manual(labels = c("Benthic", "Water","Stream","Observed"),values=c("green","blue" , "red","black"))+ labs(y = "Relative frequency")+ theme_classic()+
  geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.1, color="blue" ,linetype="dashed", size=1) + 
  geom_vline(xintercept=1.1, color="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=0.259, color="green", linetype="dashed", size=1)+
theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
  theme(legend.key.size = unit(0.5, "in"))+
  theme(legend.text=element_text(size=14))+guides(fill=guide_legend(nrow=2,byrow=TRUE))                                                                                                     

test<- read.csv("C:/git/sinnathamby_pwc/io/test.csv", header=TRUE, 
                   sep=",")
#library(lattice)
attach(test)
ggplot(test) +
geom_histogram(aes(x=max_ben, y =(..count..)/sum(..count..)),alpha=0.5,fill="green")+ 
geom_histogram(aes(x=max_h20, y =(..count..)/sum(..count..)),alpha=0.5,fill="blue")+ 
geom_histogram(aes(x=Stream, y =(..count..)/sum(..count..)), fill="red")+
geom_histogram(aes(x=Observed, y =(..count..)/sum(..count..)),alpha=0.5,fill="black")+ scale_x_log10()+
labs(x="log [Maximum concentration (ug/l)]",y = "Relative frequency")+ theme_classic()+
#geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1)+ 
#geom_vline(xintercept=0.1, color="blue" ,linetype="dashed", size=1) + 
#geom_vline(xintercept=1.1, color="black", linetype="dashed", size=1) + 
#geom_vline(xintercept=0.259, color="green", linetype="dashed", size=1)+
geom_vline(xintercept=0.17, color="black", size=1)+
theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
theme(legend.key.size = unit(0.5, "in"))+
theme(legend.text=element_text(size=14))+guides(fill=guide_legend(nrow=2,byrow=TRUE))  

sum(test$max_ben>0.17)
sum(test$max_h20>0.17)
dim(test)
# ggplot(max_con, aes(x=Concentration, color=Media,fill=Media)) + 
#   geom_histogram(alpha=0.5, position="identity")+
#   scale_x_log10()+scale_color_manual(labels = c("Benthic", "Water","Sediment"),values=c("green","blue" , "red"))+
#   scale_fill_manual(labels = c("Benthic", "Water","Sediment"),values=c("green","blue" , "red"))+ labs(y = "Frequency")+ theme_classic()+
#   geom_vline(aes(xintercept=0.04, linetype=name),show.legend =FALSE, color="red", linetype="dashed", size=1) + 
#   geom_vline(xintercept=0.1, color="blue" ,linetype="dashed", size=1) + 
#   geom_vline(xintercept=1.1, color="black", linetype="dashed", size=1) + 
#   geom_vline(xintercept=0.259, color="green", linetype="dashed", size=1)+
#   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(size=14))+ 
#   theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(size=14,colour="black" ))+
#   theme(legend.key.size = unit(0.5, "in"))+
#   theme(legend.text=element_text(size=14))+guides(fill=guide_legend(nrow=2,byrow=TRUE))    





#histogram  
ggplot(data=par_distributionm, aes(par_distributionm$value)) + geom_histogram()+geom_density(col=2)+ facet_wrap( ~ variable, scales = "free",ncol=7)

ggplot(data=par_distributionm, aes(par_distributionm$value)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")+facet_wrap( ~ variable, scales = "free",ncol=7)

ggplot(data=par_distribution, aes(par_distribution$max_h20)) +
  geom_histogram(binwidth=5, colour="black", fill="white")

 # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))


  ggplot(par_distributionm_50, aes(x=value, y=max_h20))+geom_point(alpha = 0.2)+
    facet_wrap( ~ variable, scales = "free",ncol=7)+theme_bw()+ stat_smooth(method = "loess") 

  ggplot(data=par_distributionm_50, aes(par_distributionm_50$value)) + geom_histogram()+ geom_density(col=2) +facet_wrap( ~ variable, scales = "free",ncol=7)
  
#
#+ 

ggplot(data=par_distributionm, aes(x=max_h20, y=value, shape=variable,color=variable)) +
  geom_point(alpha = 0.2)+stat_smooth()+
  theme_bw()

##pcc_plot###############################################
pcc_plot<- read.csv("C:/git/sinnathamby_pwc/figures/pcc_plot.csv", header=TRUE, 
                            sep=",")
attach(pcc_plot)
ggplot(pcc_plot, aes(x=Parameter, y=PCC,fill=Media))+geom_bar(colour="black",stat = "identity")+
  facet_wrap(~ Media,ncol=4)+theme_bw()+ theme(legend.position="none")+ coord_flip()
##############################################################################
#################################################################################
