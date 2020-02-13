# ------------------------------------------------------------------
# plot obsvered bifenthrin data (CALPIP)
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# set-up
# ------------------------------------------------------------------
# check to make sure required packages are installed
list.of.packages <- c("plyr", "dplyr", "reshape2", "ggplot2", "grid", "gridExtra", "sensitivity", "abind", 
                      "ppcor", "scales", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

# install and load library dependencies
#install.packages("magrittr")
#install.packages("sensitivity")
library(lhs)
library(magrittr)
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(sensitivity)
library(abind)
library(tools)
library(ppcor)
library("dplyr")
library(MASS)
library(dplyr)
library (scales)


# read in data
calpip_p <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/placer_09-14_with_homeowner_for_loop.csv",
                   header= TRUE, sep= ",")

calpip_s <- read.csv("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/sacramento_09-14_with_homeowner_for_loop.csv",
                     header= TRUE, sep= ",")


# add in full date column
calpip_p$date <- seq(as.Date("2009-01-01"), as.Date("2014-12-01"), by="months")#format 1961-01-01
calpip_s$date <- seq(as.Date("2009-01-01"), as.Date("2014-12-01"), by="months")#format 1961-01-01


# ------------------------------------------------------------------
# plot
# ------------------------------------------------------------------
p <- ggplot(data=calpip_p, aes(x=date, y=bif_kgha_with_home)) +
  geom_bar(stat="identity", fill="#525252") +
  scale_x_date(date_breaks="2 months",date_labels="%m-%y", limits=as.Date(c('2009-01-01', '2014-12-01'))) +
  labs(title = "", x = "", y = "Bifenthrin Application (kg/ha)", color = "") +
  theme_bw() +
  theme(legend.position = "none") 

print(p)




s <- ggplot(data=calpip_s, aes(x=date, y=bif_kgha_with_home)) +
  geom_bar(stat="identity", fill="#525252") +
  scale_x_date(date_breaks="2 months",date_labels="%m-%y", limits=as.Date(c('2009-01-01', '2014-12-01'))) +
  labs(title = "", x = "", y = "Bifenthrin Application (kg/ha)", color = "") +
  theme_bw() +
  theme(legend.position = "none") 

print(s)

















# ------------------------------------------------------------------
# the end
# ------------------------------------------------------------------