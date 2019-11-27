merced <- read.table("C:/git/sinnathamby_pwc/figures/MercedPesticides.csv", header=TRUE, 
                   sep=",")
state <- read.table("C:/git/sinnathamby_pwc/figures/StatePesticides.csv", header=TRUE, 
                     sep=",")
library(ggplot2)
library(RColorBrewer)
attach(merced)
attach(state)
#par(mfrow=c(2,1))
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


p1 <-ggplot(state,aes(x=Year,y=Pounds.x.100.000.of.pesticides.used, colour=Chemical))+theme_bw()+geom_line(size=1.5)+labs(x = "", y = "Poundsx10^5 active ingredient",color="Category")+ theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5, face="bold", colour="black",size=12))+ theme(axis.title.x = element_text(face="bold", colour="black", size=12),axis.text.x  = element_text(vjust=0.5, face="bold", colour="black",size=12))+theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank())+ scale_x_continuous(breaks=seq(1999,2014,2))
p2 <-ggplot(merced,aes(x=Year,y=Pounds.x.100.000.of.pesticides.used, colour=Chemical))+ theme_bw()+geom_line(size=1.5)+labs(x = "Year", y = "Poundsx10^5 active ingredient",color="Category")+ theme(axis.title.y = element_text(face="bold", colour="black", size=12),axis.text.y  = element_text(vjust=0.5,face="bold", colour="black", size=12))+ theme(axis.title.x = element_text(face="bold", colour="black", size=12),axis.text.x  = element_text(vjust=0.5, face="bold", colour="black",size=12))+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(colour="black", size = 12, face = "bold"))+ scale_x_continuous(breaks=seq(1999,2014,2))
multiplot(p1, p2, cols=1)
# ggplot(merced,aes(x=Year,y=Pounds.x.100.000.of.pesticides.used,fill=Category))+geom_line(ylim=c(1,1,1))+facet_grid(~Chemical)+ 
#   labs(x = "", y = "Percent Decrease from Baseline",color="Category")


ggplot(state,aes(x=Year,y=Pounds.x.100.000.of.pesticides.used, color=factor(Chemical)))+geom_point(size=3,pch=21)+facet_wrap(~Category, scale="free",ncol=1)+
  stat_smooth()+
  theme_bw()+labs(x = "", y = "Pounds x 10^5 active ingredient",color="Category")+ theme(axis.title.y = element_text(colour="black", size=12),axis.text.y  = element_text(vjust=0.5, colour="black",size=12))+ theme(axis.title.x = element_text(colour="black", size=12),axis.text.x  = element_text(vjust=0.5,colour="black",size=12))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+theme(legend.text=element_text(size=12))+scale_x_continuous(breaks=seq(1999,2014,1))+theme(strip.text.x = element_text(size =12))



scenario <- read.table("C:/git/sinnathamby_pwc/figures/scenarios.csv", header=TRUE, 
                   sep=",")
attach(scenario)
library(ggplot2)
library(RColorBrewer)

ggplot(scenario,aes(x=Crop,y=Daily.Average.Aqueous.Conc..in.Water.Columm..ug.l.,fill=Rate))+geom_boxplot(ylim=c(5,5,5))+facet_grid(~Chemical)+ 
  labs(x = "BMPs", y = "Percent of Total Flow",color="BMP")+
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_brewer(palette="BrBG")+
  theme_bw()+
  theme(strip.background=element_rect(fill="black"))+
  theme(strip.text=element_text(color="white", face="bold",size=20))+ theme(axis.title.x = element_text(face="bold", colour="black", size=20),
                                                                            axis.text.x  = element_text(angle=90, vjust=0.5, size=20))+ theme(axis.title.y = element_text(face="bold", colour="black", size=20),
                                                                                                                                              axis.text.y  = element_text(vjust=0.5, size=20,colour="black" ))+theme(legend.position = "top")+ theme(legend.key.size = unit(0.5, "in"))+theme(axis.text.x = element_blank())+theme(legend.text=element_text(size=20))+guides(fill=guide_legend(nrow=2,byrow=TRUE))                                                                                                                                              
