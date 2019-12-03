# -----------------------------------------------------------------------
# plotting sensitivity analysis -- max
# -----------------------------------------------------------------------


# -----------------------------------------------------------------------
# load data, prep for plotting 
# -----------------------------------------------------------------------


# sensitivity 
sens <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_przm.csv", header=TRUE, 
                   sep=",")

sens$Pesti_f = factor(sens$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))


# sensitivity wc
sens_wc <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_wc.csv", header=TRUE, 
                      sep=",")
sens_wc$Pesti_f = factor(sens_wc$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))


# sensitivity bc
sens_bc <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_bc.csv", header=TRUE, 
                      sep=",")
sens_bc$Pesti_f = factor(sens_bc$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))


# sensitivity alf
sens_alf <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_przm_alf.csv", header=TRUE, 
                      sep=",")
#sens_alf$Pesti_f = factor(sens_alf$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))


# sensitivity wc alfa
sens_wc_alf <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_wc_alfa.csv", header=TRUE, 
                         sep=",")
#sens_wc_alf$Pesti_f = factor(sens_wc_alf$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))


# sensitiivty bc alfa
sens_bc_alf <- read.table("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/sensitivity_bc_alfa.csv", header=TRUE, 
                          sep=",")
#sens_bc_alf$Pesti_f = factor(sens_bc_alf$Pesti, levels=c('Diazinon','Chlorpyrifos','Malathion'))




# -----------------------------------------------------------------------
# plot
# -----------------------------------------------------------------------

# sensitivity
ggplot(sens, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti_f,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# sensitivity wc
ggplot(sens_wc, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti_f,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# sensitivity bc
ggplot(sens_bc, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti_f,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# sensitivity alf
ggplot(sens_alf, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# sensitivity wc alfa
ggplot(sens_wc_alf, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# sensitivity bc alfa
ggplot(sens_bc_alf, aes(x = Parameter, y = pcc, fill=Source)) +
  geom_bar(position="dodge", width=0.75,stat = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="top")+
  facet_wrap(~Pesti,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=14, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))+
  theme(strip.background=element_blank())+geom_vline(aes(xintercept=0))+
  theme(strip.text=element_blank())+ 
  theme(axis.title.x = element_text( colour="black", size=14),axis.text.x  = element_text(angle=90, vjust=0.5, size=14,colour="black"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black"))+
  theme(legend.title=element_blank())+labs(x = "Parameters", y = "PCC")+ theme(legend.text=element_text(size=14))


# -----------------------------------------------------------------------
# the end
# -----------------------------------------------------------------------