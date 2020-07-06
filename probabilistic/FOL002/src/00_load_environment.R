# -----------------------------------------------------------------------
# PWC probabilistic evaluation
# -----------------------------------------------------------------------


# -----------------------------------------------------------------------
# set-up
# -----------------------------------------------------------------------

# check to make sure required packages are installed
list.of.packages <- c("plyr", "dplyr", "reshape2", "ggplot2", "grid", "gridExtra", "sensitivity", "abind", 
                      "ppcor", "scales", "MASS", "cowplot", "egg")
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

# echo environment
Sys.info()
Sys.info()[4]
.Platform
version


# set some default directories based on machine location
# Tom's mac air
if(Sys.info()[4]=="stp-air"){
  pwcdir <- "~/git/yuan_urban_pesticides/"
}
# Tom's epa laptop
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  pwcdir <- "c:/git/yuan_urban_pesticides/"
  # pwc,przm (without directory, the file needs to be in vpdir_exe above)
  przm_filename <- "PRZM5.inp"
}
#Sumathy
if(Sys.info()[4]=="LZ2032ESSINNATH"){
  pwcdir <- "C:/Users/SSINNATH/Documents/git/yuan_urban_pesticides/"
  # pwc,przm (without directory, the file needs to be in vpdir_exe above)
  przm_filename <- "PRZM5.inp"
}
# Emma
if(Sys.info()[4]=="LZ2626UECHELSVI"){
  pwcdir <- "C:/Users/echelsvi/git/yuan_urban_pesticides/probabilistic/FOL002/"
}
print(paste("Root directory location: ", pwcdir, sep=""))


# subdirectories
pwcdir_input <- paste(pwcdir, "input/", sep = "")
pwcdir_output <- paste(pwcdir, "output/", sep = "")
pwcdir_log <- paste(pwcdir, "log/", sep = "")
pwcdir_fig <- paste(pwcdir, "figures/", sep = "")
pwcdir_exe <- paste(pwcdir, "exe/", sep = "")
przmdir_exe <- paste(pwcdir, "exe/", sep = "")
vvwmdir_exe <- paste(pwcdir, "exe/", sep = "")
pwcdir_io <- paste(pwcdir, "io/", sep = "")
pwcdir_in_przm <- paste(pwcdir_input, "przm/", sep = "")
pwcdir_in_vvwm <- paste(pwcdir_input, "vvwm/", sep = "")
pwcdir_out_przm <- paste(pwcdir_output, "przm/", sep = "")
pwcdir_out_vvwm <- paste(pwcdir_output, "vvwm/", sep = "")
pwcdir_weather <- paste(pwcdir, "weather/", sep = "")
pwcdir_sobol <- paste(pwcdir, "sobol/", sep = "")

# pwc executable version -- pwc 1.59
#in order for scripts to work, must be using PWC version 1.59 (2018)
pwc_binary<- "PesticideWaterCalculator.exe"
pwcdir_executable <- paste(pwcdir_exe, pwc_binary, sep="")
przm_binary<- "PRZM5.exe"
przmdir_executable <- paste(przmdir_exe, przm_binary, sep="")
vvwm_binary<- "VVWM.exe"
vvwmdir_executable <- paste(vvwmdir_exe, vvwm_binary, sep="")


# -----------------------------------------------------------------------
# the end
# -----------------------------------------------------------------------