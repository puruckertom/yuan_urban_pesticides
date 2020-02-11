# ---------------------------------------------------------------
# run deterministic pwc
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


con_przm5 <- file(paste(determdir,"input/FOL002/exceed_app_rates/PRZM5",".inp",sep=""))
a_old=readLines(con_przm5)
a=readLines(con_przm5)
close(con_przm5)
  
# -------------- update the files (weather file, PRZM output file) ----------
#a[4]= paste(pwcdir_output,"17484_grid_folsom.wea")
a[6]= paste(determdir,"input/FOL002/exceed_app_rates/output.zts")
  

  
# ------------------------------------------------------------------------------
# create a przm input (will have one for each of the simulations = 5000)
# ------------------------------------------------------------------------------
newdir <- paste0(determdir,"input/FOL002/exceed_app_rates/outputs")
print(newdir)
dir.create(newdir,showWarnings = FALSE) 
cwd <- getwd() #current directory
setwd(newdir) 
  
# copy weather file
weather_input <- paste(determdir, "input/FOL002/exceed_app_rates/17484_grid_folsom.wea", sep="")
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
con <- file(paste(determdir, "input/FOL002/exceed_app_rates/vvwmTransfer",".txt",sep=""))
l_old=readLines(con)
l=readLines(con)
close(con)

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
# source(paste(pwcdir,"src/02write_przm_input.R",sep = ""))

# #run###PRZM.exe#
system2(przmdir_executable)
  
# #run###vvwm.exe#
system2(vvwmdir_executable, "vvwmTransfer.txt")



# ----------------------------------------------------------------------------
# the end
# ----------------------------------------------------------------------------