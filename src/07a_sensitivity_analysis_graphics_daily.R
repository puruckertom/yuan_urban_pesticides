# -----------------------------------------------------------------------
# plotting sensitivity analysis - daily
# -----------------------------------------------------------------------



# -----------------------------------------------------------------------
# multiple plot function
# -----------------------------------------------------------------------

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#   - cols:   Number of columns in layout
#   - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
#           If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#           then plot 1 will go in the upper left, 2 will go in the upper right, and
#           3 will go all the way across the bottom.


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# -----------------------------------------------------------------------
# load data, call variables
# -----------------------------------------------------------------------

ndays <- length(timearray)

date <- 34:ndays


# pcc daily results
load(paste(pwcdir, "io/tarray_pwc_pcc_out_daily.RData", sep = ""))
dim(tarray_pwc_pcc_out_daily)

colnames(tarray_pwc_pcc_out_daily) <- colnames(inputs_lhs)
colnames(tarray_pwc_pcc_out_daily) 



# -----------------------------------------------------------------------
# plot 
# -----------------------------------------------------------------------


# ------------------------------------
# plot control daily sensitivities 
# ------------------------------------

pcc_day <- as.data.frame(cbind(date, tarray_pwc_pcc_out_daily[34:ndays,1:dim(tarray_pwc_pcc_out_daily)[2]]))
colnames(pcc_day) <- c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","CN_f","uslec_c","uslec_f","MNGN",
                       "depth","COVMAX","HTMAX","holdup","bd1","bd2","bd3","bd4","bd5","fc","WP","OC","dep","app_rate","app_eff",
                       "DWRATE","DSRATE","koc","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                       "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")

cont<- pcc_day%>% dplyr::select(one_of(c("date","PFAC","ANETD","uslek","uslels","uslep","slp","hl","CN_c","CN_f","uslec_c","uslec_f","MNGN",
                                         "depth","COVMAX","HTMAX","holdup","bd1","bd2","bd3","bd4","bd5","fc","WP","OC","dep","app_rate","app_eff",
                                         "DWRATE","DSRATE","koc","aer_aq","temp_ref_aer","anae_aq","temp_ref_anae","photo","RFLAT","hydro",
                                         "SOL","benthic_depth","porosity","bulk_density","FROC2","DOC2","BNMAS","SUSED","CHL","FROC1","DOC1","PLMAS","bf")))

melted_pwc = melt(cont, id.vars="date")
#melted_pwc<- na.omit(melted_pwc)


# x scale option 1 ?
daily_pcc_sensitivity <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc$variable)) +facet_wrap(~variable, scale="free")

  guides(fill=FALSE) +  
  xlab("Simulation Day") + 
  ylab("Partial Correlation Coefficient") +
  annotate("text", x = 1000, y = 0.92, label = "AveCon H20", size=6) +
  theme_bw() +
  scale_x_discrete(breaks = c(61,426,610,791,1035)) +
  theme(legend.position = "none",  axis.title.x=element_blank(), axis.text.x=element_blank())

  # x scale option 2 ?
daily_pcc_sensitivity <- ggplot(melted_pwc, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc$variable))+scale_x_continuous(limits = c(1, 365))




# ------------------------------------
# plot daily high sensitive paramters only
# ------------------------------------

cont1<- pcc_day%>%select(one_of(c("date","CN_c","bd1","fc","WP","app_rate","app_eff","hydro","bulk_density","FROC2")))

melted_pwc1 = melt(cont1, id.vars="date")
#melted_pwc<- na.omit(melted_pwc)


a <- ggplot(melted_pwc1, aes(x=date, y=value, group=variable)) +
  geom_line(aes(colour=melted_pwc1$variable),size=1)+scale_x_continuous(limits = c(1097, 1827))+ scale_y_continuous(breaks=seq(-1,1,by=0.5), limits=c(-1,1))+ 
  theme_bw()+labs(title = "", x = "", y = "PCC", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  #theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))+
  theme(legend.position = "none")


# ------------------------------------
# plot ????
# ------------------------------------
prec<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/watershed1/precip.csv", header=TRUE, 
                    sep=",")
prec$Date <- as.Date(prec$Date, "%m/%d/%Y")

#prec23<-subset(prec, prec$Date> 1/1/2002 & prec$Date < 1/1/2003)
prec23<-prec %>%filter(Date >"2002-01-01", Date <"2004-01-01") #comma same as &

P <-ggplot(prec23, aes(x=Date,y=PRCP0))+geom_line()+
  theme_bw()+labs(title = "", x = "", y = "Precipitation (cm)", color = "")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
  theme(legend.text=element_text(size=12))+
  theme(panel.grid.major = element_line(colour="gray", size = (0.25)),panel.grid.minor = element_line(size = (0.25), colour="gray"))

multiplot(daily_pcc_sensitivity_hs, P, cols=1)





gA <- ggplotGrob(a)
gB <- ggplotGrob(P)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1)
# p <- ggplot(melted_pwc1, aes(factor(melted_pwc1$variable), melted_pwc1$value)) 
# p + geom_point() + facet_wrap(~melted_pwc1$variable, scale="free")



# ------------------------------------
# plot ????
# ------------------------------------
dia_con<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/watershed1/diazinon0203.csv", header=TRUE, 
                        sep=",")

ggplot(melted_pwc1, aes(x=date, y=melted_pwc1$value, group=melted_pwc1$variable)) +
  geom_line(aes(colour=melted_pwc1$variable))+scale_x_continuous(limits = c(1097, 1826))+
  guides(fill=FALSE) +  
  #xlab("Number of Simulation Day from 1999") + 
  #ylab("Partial Correlation Coefficient") +
  theme_classic()+labs(title = "", x = "Number of Simulation Day from 1999", y = "Partial Correlation Coefficient", color = "Sensitive parameters")+ 
  theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
   theme(legend.text=element_text(size=12))
 
ggplot(melted_pwc1, aes(x=date, y=melted_pwc1$value, group=melted_pwc1$variable)) +
   geom_line(aes(colour=melted_pwc1$variable))+
   guides(fill=FALSE) +  
   #xlab("Number of Simulation Day from 1999") + 
   #ylab("Partial Correlation Coefficient") +
   theme_classic()+labs(title = "", x = "Number of Simulation Day from 1999", y = "Partial Correlation Coefficient", color = "Sensitive parameters")+ 
   theme(axis.title.x = element_text(colour="black", size=14),axis.text.x  = element_text(colour="black", vjust=0.5, size=14))+ theme(axis.title.y = element_text(colour="black", size=14),axis.text.y  = element_text(vjust=0.5, size=14,colour="black" ))+
   theme(legend.text=element_text(size=12))



# ------------------------------------
# plot ????
# ------------------------------------
dia_sens<- read.csv("C:/Users/SSinnath/Research/CAVernalPools/vvwm/diazinon/watershed1/sensitivity.csv", header=TRUE, 
                   sep=",")

ggplot(dia_sens, aes(x = Parameter, y = PCC)) +
 geom_bar(stat = "identity",position = "identity") + #scale_y_continuous(limits = c(-1, 1))+
  #scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different colors
  theme_bw()+
  theme(legend.position="none")+
  labs(y="PCC", x="")+facet_wrap(~Media,ncol =1)+ #scales="free", 
  theme(axis.text.x = element_text(size=12, angle=90),axis.text.y = element_text(size=14))+ geom_hline(aes(yintercept=0))
  


# --------------------------------------------------------------------------------------
# the end
# --------------------------------------------------------------------------------------