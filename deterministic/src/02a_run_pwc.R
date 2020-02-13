# ---------------------------------------------------------------
# run deterministic pwc -- FOL002
# ---------------------------------------------------------------

# Note: must be using PWC version 1.59 (2018) for scripts to work.
#       Version 1.59 output/input files have a specific format, 
#       and these scripts are intended to read this format. 
#       Version 1.52 or 1.53 file formats will not work with these
#       scripts.
#
#       Version 1.59 produces a fort.78 file with PRZM, and this
#       file is used in VVWM.  (Previous versions (1.52/1.53), 
#       typically produce fort.13. This is not compatible with 
#       these scripts because these scripts are built off of 
#       Version 1.59 files.)
#
#       The executables located in the /exe/ folder and the
#       GUI downloaded on your PC must both be Version
#       1.59 (2018). 


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

# echo environment
Sys.info()
Sys.info()[4]
.Platform
version

# root directory - Emma
if(Sys.info()[4]=="LZ2626UECHELSVI"){
  determdir <- "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/"
}
print(paste("Root directory location: ", determdir, sep=""))


con_przm5 <- file(paste(determdir,"input/FOL002/PRZM5",".inp",sep=""))
a_old=readLines(con_przm5)
a=readLines(con_przm5)
close(con_przm5)
  
# update the files (weather file, PRZM output file)
a[4]= paste(determdir,"input/FOL002/17484_grid_folsom.wea",sep="")
a[6]= paste(determdir,"input/FOL002/outputs/output.zts",sep="")
  

# create a przm input
newdir <- paste0(determdir,"input/FOL002/outputs")
print(newdir)
dir.create(newdir,showWarnings = FALSE) 
cwd <- getwd() #current directory
setwd(newdir) 
  
# copy weather file
weather_input <- paste(determdir, "input/FOL002/17484_grid_folsom.wea", sep="")
file.copy(weather_input,newdir, recursive = FALSE, 
            copy.mode = TRUE)
  
file_path_as_absolute(newdir)
  
# update weather and output under each input folder
a[4]=paste(file_path_as_absolute(newdir),"/","17484_grid_folsom.wea", sep="")
a[6]=paste(file_path_as_absolute(newdir),"/","output.zts", sep="")
  
# copy PRZM.exe
przmdir_executable <- paste(determdir, "exe/PRZM5.exe", sep="")
print(paste(file.exists(przmdir_executable), ": executable file at", przmdir_executable))
file.copy(przmdir_executable,newdir, recursive = FALSE, 
          copy.mode = TRUE)


# read in the VVWM output dummy file
con <- file(paste(determdir, "input/FOL002/vvwmTransfer",".txt",sep=""))
l_old=readLines(con)
l=readLines(con)
close(con)

# update file names
l[1]=paste(file_path_as_absolute(newdir),"/","output", sep="")
l[30]=paste(file_path_as_absolute(newdir),"/17484_grid_folsom.wea", sep="")
l[69]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Parent_daily.csv","\"", sep="")#
l[70]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate1_daily.csv","\"", sep="")#
l[71]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate2_daily.csv","\"", sep="")#
l[72]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Parent.txt","\"", sep="")#
l[73]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate1.txt","\"", sep="")#
l[74]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate2.txt","\"", sep="")#
l[75]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Parent_DEEM.rdf","\"", sep="")#
l[76]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate1_DEEM.rdf","\"", sep="")#
l[77]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate2_DEEM.rdf","\"", sep="")#
l[78]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Parent_Calendex.rdf","\"", sep="")#
l[79]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate1_Calendex.rdf","\"", sep="")#
l[80]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_Degradate2_Calendex.rdf","\"", sep="")#
l[81]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_15_Parent.txt","\"", sep="")#
l[82]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_15_Degradate1.txt","\"", sep="")#
l[83]=paste("\"",file_path_as_absolute(newdir),"/","output_FOL002_parent_only_Custom_15_Degradate2.txt","\"", sep="")#

# write out the file 
vvwm_file <- paste("vvwmTransfer",".txt", sep="")
file.exists(vvwm_file)
file.create(vvwm_file)
file.exists(vvwm_file)
con_vvwm <-file(vvwm_file)
writeLines(l, con_vvwm)
close(con_vvwm)



# copy VVWM.exe
vvwmdir_executable <- paste(determdir, "exe/VVWM.exe", sep="")
print(paste(file.exists(vvwmdir_executable), ": executable file at", vvwmdir_executable))
file.copy(vvwmdir_executable,newdir, recursive = FALSE, 
          copy.mode = TRUE)
  
# write input file
przm_file <- paste("PRZM5",".inp", sep="")
file.exists(przm_file)
file.create(przm_file)
file.exists(przm_file)
con_przm <-file(przm_file)
writeLines(a, con_przm)
close(con_przm)

# #run###PRZM.exe#
system2(przmdir_executable)
  
# #run###vvwm.exe#
system2(vvwmdir_executable, "vvwmTransfer.txt")



# ----------------------------------------------------------------------------
# the end
# ----------------------------------------------------------------------------