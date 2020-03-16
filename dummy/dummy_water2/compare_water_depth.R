# ---------------------------------------------------------------------------
# read deterministic model outputs
# ---------------------------------------------------------------------------

# ran PWC with 02_run_pwc source code.
# output files from each site's model run were placed in each site's input/ folder.
# the pwc output files from each site's model run are utilized in this code.



# ---------------------------------------------------------------------------
# load environment
# --------------------------------------------------------------------------- 

# echo environment
Sys.info()
Sys.info()[4]
.Platform
version

# root directory - Emma
if(Sys.info()[4]=="LZ2626UECHELSVI"){
  determdir <- "C:/Users/echelsvi/git/yuan_urban_pesticides/dummy/dummy_water2/outputs/"
}
print(paste("Root directory location: ", determdir, sep=""))





# check to make sure required packages are installed
list.of.packages <- c("plyr", "dplyr", "reshape2", "ggplot2", "grid", "gridExtra", "abind", 
                      "ppcor", "scales", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

# install and load library dependencies
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(abind)
library(tools)
library(ppcor)
library(MASS)
library(dplyr)
library (scales)

# ---------------------------------------------------------------------------
# site: FOL002
# --------------------------------------------------------------------------- 

# read pwc output file
pwc_fol002 <- read.csv(paste(determdir,"output_FOL002_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                       skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_fol002) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
fol002_h2_df <- data.frame(matrix(data=NA, ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_h2_df) <- c("date", "h2_fol002")
fol002_h2_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_h2_df$h2_fol002 <- (pwc_fol002[,2]*1000000)#in ug/ml




# read pwc output file
pwc_fol002_real <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/input/FOL002/outputs/output_FOL002_parent_only_Custom_Parent_daily.csv", header= FALSE, sep= ",",
                       skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_fol002_real) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
fol002_h2_df_real <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol002_real)))
colnames(fol002_h2_df_real) <- c("date", "h2_fol002")
fol002_h2_df_real$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002_real)-1)
fol002_h2_df_real$h2_fol002 <- (pwc_fol002_real[,2]*1000000)#in ug/ml




# compare constant depth vs. varying depth
emma_plot <- ggplot() +
  geom_line(data=fol002_h2_df, aes(x=date, y=h2_fol002, color="constant water depth"), linetype="solid", size=1) +
  geom_line(data=fol002_h2_df_real, aes(x=date, y=h2_fol002, color="varying water depth"), linetype="solid", size=1) +
  
  scale_x_date(date_breaks="1 year", date_labels="%m-%d-%y", limits=as.Date(c('2009-01-01', '2014-12-31'))) +
  scale_y_continuous(trans="log10", breaks=trans_breaks("log10", function(x) 10^x), 
                     labels=trans_format("log10", math_format(10^.x)), limits=c(NA,5)) 
print(emma_plot)  


# plot constant depth
fol002_h2_df_water <- data.frame(matrix(data=NA, ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_h2_df_water) <- c("date", "water_depth")
fol002_h2_df_water$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_h2_df_water$water_depth <- (pwc_fol002[,1])

water_plot1 <- ggplot() +
  geom_line(data=fol002_h2_df_water, aes(x=date, y=water_depth), linetype="solid")
print(water_plot1)


# plot varying depth
fol002_h2_df_water_real <- data.frame(matrix(data=NA, ncol = 2, nrow = nrow(pwc_fol002_real)))
colnames(fol002_h2_df_water_real) <- c("date", "water_depth")
fol002_h2_df_water_real$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002_real)-1)
fol002_h2_df_water_real$water_depth <- (pwc_fol002_real[,1])

water_plot2 <- ggplot() +
  geom_line(data=fol002_h2_df_water_real, aes(x=date, y=water_depth), linetype="solid")
print(water_plot2)

