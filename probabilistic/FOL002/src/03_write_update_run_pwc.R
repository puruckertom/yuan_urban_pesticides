# ---------------------------------------------------------------
# create empty dataframe for outputs
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




# ---------------------------------------------------------------
# PRZM5 Output File (Time Series)
# ---------------------------------------------------------------

# read in PRZM output - simulated pesticide transport through soil, for each time date
df_przm <- read.table(paste(pwcdir,"output/output",".zts", sep=""), header= FALSE, sep= "",
                skip = 3, stringsAsFactors = FALSE, row.names=NULL)

# obtain structural dimensions
dim(df_przm)
str(df_przm)
nrows_przm <- dim(df_przm)[[1]]
ncols_przm <- dim(df_przm)[[2]]

# create blank array with these dim
outputdf <- array(data=NA, c(nrows_przm,ncols_przm,Nsims))
dim(outputdf)


# ---------------------------------------------------------------
# VVWM Output File
# ---------------------------------------------------------------

# read in VVWM output - simulated water body processes, for each time date
pwcdf_o <- read.csv(paste(pwcdir,"output/output_FOL002_parent_only_Custom_Parent_daily.csv", sep=""), header= FALSE, sep= ",",
                  skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# obtain structural dimensions
dim(pwcdf_o)
str(pwcdf_o)
rows_pwc <- dim(pwcdf_o)[[1]]
cols_pwc <- dim(pwcdf_o)[[2]]

# create blank array with these dim
pwcoutdf <- array(data=NA, c(rows_pwc,cols_pwc,Nsims))


# ---------------------------------------------------------------
# conversion factor dataframe
# ---------------------------------------------------------------

# create dataframe - to be filled w/ conversion factors
con_fac <- data.frame(matrix(ncol = 1, nrow = Nsims))
dim(con_fac)


# ---------------------------------------------------------------
# update PRZM input file
# ---------------------------------------------------------------


set.seed(42)

# we will run the PWC PRZM 5000 times (5000 = our # of simulations)
# to run each of the 5000 PWC simulations, we will update the PWC PRZM input file with the corresponding Latin Hypercube parameter quanitities
for(Ite in 1:Nsims){
  print(Ite)
  con_przm5 <- file(paste(pwcdir,"input/przm/PRZM5",".inp",sep=""))
  a_old=readLines(con_przm5)
  a=readLines(con_przm5)
  close(con_przm5)

  # -------------- update the files (weather file, PRZM output file) ----------
  a[4]= paste(pwcdir_output,"17484_grid_folsom.wea")
 
  a[6]= paste(pwcdir_output,"output.zts")
 
  # --------------------------- Pan factor ------------------------------------
  # recall: input_list is a uniformly distr. latin hypercube (5000*50 = nsim*nparam)
  
  # round each PFAC (one of our param) to 2 decimals
  PFAC=round(input_list[Ite,"PFAC"],2)
  # update the input file's Record 1 quantities
  a[10]=paste(sprintf("%.02f", PFAC),substr(a[10],5,16), sep="")
  
  # ------------------------ Evaporation Depth (cm) ---------------------------
  
  # round each ANETD to 1 decimal
  ANETD=round(input_list[Ite,"ANETD"],1)
  
  # update the ANETD quantity for input file's Record 1
  ANETD_list <- unlist(strsplit(a[10],","))
  ANETD_list[3]<-ANETD
  a[10]=paste(ANETD_list,collapse=",")
  
  
  # --------------------------- USLEK -----------------------------------------
  
  # round each USLEK to 2 decimals
  uslek=round(input_list[Ite,"uslek"],2)
  
  # update the input file's Record 3 for USLEK
  a[14]=paste(sprintf("%.02f", uslek),substr(a[14],5,34), sep="")
  
  
  # --------------------------- USLELS ---------------------------------------- 
  # note: slope length factor (the effect of slope length on erosion) 
  
  # round each USLELS to 2 decimals
  uslels=round(input_list[Ite,"uslels"],2)
  
  # update the input file's Record 3 for USLELS
  uslels_list <- unlist(strsplit(a[14],","))
  uslels_list[2]<-uslels
  a[14]=paste(uslels_list,collapse=",")
  
  
  # --------------------------- USLEP ------------------------------------------ 
  
  # round each USLEP to 2 decimals
  uslep=round(input_list[Ite,"uslep"],2)
  
  # update the input file's Record 3 for USLEP
  uslep_list <- unlist(strsplit(a[14],","))
  uslep_list[3]<-uslep
  a[14]=paste(uslep_list,collapse=",")
  
  
  # --------------------------- Slope ------------------------------------------ 
  
  # round each slp to 2 decimals
  slp=round(input_list[Ite,"slp"],3)
  
  # update the input file's Record 3 for slope
  slp_list <- unlist(strsplit(a[14],","))
  slp_list[6]<-slp
  a[14]=paste(slp_list,collapse=",")
  
  
  # --------------------------- Hydraulic Length -------------------------------
  
  # round each hl to 0 decimals
  hl=round(input_list[Ite,"hl"],0)
  
  # update the input file's Record 3 for hydraul length
  hl_list <- unlist(strsplit(a[14],","))
  hl_list[7]<-hl
  a[14]=paste(hl_list,collapse=",")
 
  
  # ------------------------------ CN ------------------------------------------
  
  # number of hydro-event changes that follow (Record 4, total = 12)
  Num=12
  
  # round each CN_c to 0 decimals
  CN_c=round(input_list[Ite,"CN_c"],0)
  
  # for each of the CN vals (8:24) in the input file, update the input file's Record 5 for CN
  row_0=18
  for (i in 1:Num){
    row_t=row_0+(i-1)
    cn_list <- unlist(strsplit(a[row_t],","))
    #cn_list[6]<-(as.numeric(CNPer[Ite]))*(as.numeric(cn_list[6]))
    cn_list[6]<-CN_c
    a[row_t]=paste(cn_list,collapse=",")
  }

  
  
 # ------------------------------ USLEC ----------------------------------------
 
 # number of hydro-event changes that follow (Record 4, total = 12)
 Num=12
 
 # round each uslec_c to 3 decimals
 uslec_c=round(input_list[Ite,"uslec_c"],3)
 
 # for each of the C vals (8:24) in the input file, update the input file's Record 5 for C
 row_0=18
 for (i in 1:Num){
   row_t=row_0+(i-1)
   uslec_list <- unlist(strsplit(a[row_t],","))
   uslec_list[4]<-uslec_c
   a[row_t]=paste(uslec_list,collapse=",")
   #a[row_t]=paste(substr(a[row_t],1,71), sprintf("%.02f",uslec), substr(a[row_t],76,92),sep="")
 }
 
 
 
 # ------------------------ Manning's n ------------------------------------------
 
 # number of hydro-event changes that follow (Record 4, total = 12)
 Num_r5=12
 
 # round each MNGN to 3 decimals
 MNGN=round(input_list[Ite,"MNGN"],3)
 
 # for each n in the input file, update the input file's Record 5 for n 
 row_r5=18
 for (i in 1:Num_r5){
   row_t=row_r5+(i-1)
   MNGN_list <- unlist(strsplit(a[row_t],","))
   MNGN_list[5]<-MNGN
   a[row_t]=paste(MNGN_list,collapse=",")
   #a[row_t]=paste(substr(a[row_t],1,71), sprintf("%.02f",uslec), substr(a[row_t],76,92),sep="")
 }
 
 # # ------------------------ root depth ------------------------------------------
 # 
 #  # number of crop periods that follow (Record 6, total = 7)
 #  Numd=7
 #  
 #  # round each depth to 0 decimals
 #  depth=round(input_list[Ite,"depth"],1)
 #  
 #  # for each depth in the input file, update the input file's Record 7 for depth
 #  row_0=33
 #  for (i in 1:Numd){
 #    row_t=row_0+(i-1)
 #    depth_list <- unlist(strsplit(a[row_t],","))
 #    depth_list[10]<-depth
 #    a[row_t]=paste(depth_list,collapse=",")
 #  }
 # 

 # ------------------------ covmax ---------------------------------------------
 
  # # number of crop periods that follow (Record 6, total = 7)
  # Numd=7
  # 
  # # round each COVMAX to 3 decimals
  # COVMAX=round(input_list[Ite,"COVMAX"],3)
  # 
  # # for each cover in the input file, update the input file's Record 7 for cover
  # row_0=33
  # for (i in 1:Numd){
  #   row_t=row_0+(i-1)
  #   COVMAX_list <- unlist(strsplit(a[row_t],","))
  #   COVMAX_list[11]<-COVMAX
  #   a[row_t]=paste(COVMAX_list,collapse=",")
  # }
 
 
 
 # ------------------------ htmax ---------------------------------------------
 
  # # number of crop periods that follow (Record 6, total = 7)
  # Numd=7
  # 
  # # round each HTMAX to 0 decimals
  # HTMAX=round(input_list[Ite,"HTMAX"],0)
  # 
  # # for each height in the input file, update the input file's Record 7 for height
  # row_0=33
  # for (i in 1:Numd){
  #   row_t=row_0+(i-1)
  #   HTMAX_list <- unlist(strsplit(a[row_t],","))
  #   HTMAX_list[12]<-HTMAX
  #   a[row_t]=paste(HTMAX_list,collapse=",")
  # }
  # 
 # ------------------------  holdup ---------------------------------------------
 
  # # number of crop periods that follow (Record 6, total = 7)
  # Numd=7
  # 
  # # round each holdup to 2 decimals
  # holdup=round(input_list[Ite,"holdup"],2)
  # 
  # # for each holdup in the input file, update the input file's Record 7 for holdup
  # row_0=33
  # for (i in 1:Numd){
  #   row_t=row_0+(i-1)
  #   holdup_list <- unlist(strsplit(a[row_t],","))
  #   holdup_list[13]<-holdup
  #   a[row_t]=paste(holdup_list,collapse=",")
  # }

 
  # ------------------------ bulk density ----------------------------------------

  # round bd to 2 decimals
  bd1=round(input_list[Ite,"bd1"],2)  #only bd1 because 1 horizon (Record 14, horizons = 1)

  # update input file's Record 15 for bd
  bd1_list <- c(unlist(strsplit(a[48],",")),"")#adding extra empty value at end
  bd1_list[5]<-bd1
  a[48]=paste(bd1_list,"",collapse=",")
  #a[48]=paste(substr(a[48],1,15), sprintf("%.02f",bd1), substr(a[48],20,55),sep=",")
 
 
 # -------------------------------- fc ----------------------------------------
 
  # round each fc to 2 decimals
  fc=round(input_list[Ite,"fc"],3)
 
  # for each FC in input file, update input file's Record 15 for FC
  fc_list <- unlist(strsplit(a[48],","))
  fc_list[7]<-fc
  a[48]=paste(fc_list,collapse=",")

 
 # -------------------------------- wp ----------------------------------------
 
  # round each WP to 2 decimals
  WP=round(input_list[Ite,"WP"],2)
 
  # for each WP in input file, update input file's Record 15 for WP
  WP_list <- unlist(strsplit(a[48],","))
  WP_list[8]<-WP
  a[48]=paste(WP_list,collapse=",")
   

 # -------------------------------- oc ----------------------------------------
  # number of horizons (soil layer) (Record 14)
  Num_s=1 
 
  # round each OC to 2 decimals
  OC=round(input_list[Ite,"OC"],2)
 
  # for each oc in input file, update input file's Record 15 for oc
  row_s=48
  for (i in 1:Num_s){
   row_t=row_s+(i-1)
   OC_list <- unlist(strsplit(a[row_t],","))
   OC_list[9]<-OC
   a[row_t]=paste(OC_list,collapse=",")
   
  }
 
 
 ## -------------------------- app tm -----------------------------------------
 # Num=50#Number of Applications
 # app_tm=round(input_list[Ite,"app_tm"],0)
 # row_0=57
 # for (i in 1:Num){
 #   row_t=row_0+(i-1)
 #   app_tm_list <- unlist(strsplit(a[row_t],","))
 #   app_tm_list[2]<-app_tm
 #   a[row_t]=paste(app_tm_list,collapse=",")
 #   
 # }
 #
 ## -------------------------- app tm -----------------------------------------
 # Num=50#Number of Applications
 # app_m=round(input_list[Ite,"app_tm"],0)
 # row_0=57
 # for (i in 1:Num){
 #   row_t=row_0+(i-1)
 #   app_m_list <- unlist(strsplit(a[row_t],","))
 #   app_m_list[4]<-app_m
 #   a[row_t]=paste(app_m_list,collapse=",")
 #   
 # }
 
 
 # # -------------- depth of pesticide application ------------------------------
 # 
 # # number of applications (Record C1, total = 2191)
 # Num=2191 
 # 
 # # round each dep to 2 decimals
 # dep=round(input_list[Ite,"dep"],2)
 # 
 # # for each dep in input file, update input file's Record C2 for dep
 # row_0=57
 # for (i in 1:Num){
 #   row_t=row_0+(i-1)
 #   dep_list <- unlist(strsplit(a[row_t],","))
 #   dep_list[5]<-dep
 #   a[row_t]=paste(dep_list,collapse=",")
 # 
 # }
 
 # -------------------- application rate  -------------------------------------
 
 # number of applications (Record C1, total = 2191)
 Num=2191 
 
 # round each app_rate to 5 decimals
 app_rate=round(input_list[Ite,"app_rate"],5)
 
 # for each rate in input file, update input file's Record C2 for rate
 row_0=57
 for (i in 1:Num){
   row_t=row_0+(i-1)
   app_rate_list <- unlist(strsplit(a[row_t],","))
   current_app_rate <- as.numeric(app_rate_list[6])
   app_rate_list[6] <-round((app_rate*current_app_rate), 10)
   a[row_t]=paste(app_rate_list,collapse=",")

 }
 
 
 # # -------------------- application eff  -------------------------------------
 # 
 # # number of applications (Record C1, total = 2191)
 # Num=2191 
 # 
 # # round each app_eff to 2 decimals
 # app_eff=round(input_list[Ite,"app_eff"],2)
 # 
 # # for each eff in input file, update input file's Record C2 for eff
 # row_0=57
 # for (i in 1:Num){
 #   row_t=row_0+(i-1)
 #   app_eff_list <- unlist(strsplit(a[row_t],","))
 #   app_eff_list[7]<-app_eff
 #   a[row_t]=paste(app_eff_list,collapse=",")
 #   
 # }
 ## -------------------- PLVKRT ----------------------------------------------
 # PLVKRT=input_list[Ite,"PLVKRT"]
 # PLVKRT_list <- unlist(strsplit(a[101],","))
 # PLVKRT_list[1]<-PLVKRT
 # a[101]=paste(PLVKRT_list,collapse=",")
 
 
 # # -------------------- PLDKRT -----------------------------------------------
 # 
 # # round each PLDKRT to 2 decimals
 # PLDKRT=round(input_list[Ite,"PLDKRT"],2)
 # 
 # # update input file's Record C4
 # PLDKRT_list <- unlist(strsplit(a[101],","))
 # PLDKRT_list[2]<-PLDKRT
 # a[101]=paste(PLDKRT_list,collapse=",")
 
 
 # -------------------- DWRATE -----------------------------------------------
 
 # number of soil layer
 Numhz=1 
 
 # for each dwrate, update input file's (Record C8 ?)
 DWRATE=input_list[Ite,"DWRATE"]
 row_hz=2265
 for (i in 1:Numhz){
   row_t=row_hz+(i-1)
   DWRATE_list <- unlist(strsplit(a[row_t],","))
   DWRATE_list[1]<-DWRATE
   a[row_t]=paste(DWRATE_list,collapse=",")
 }
 # -------------------- DSRATE ------------------------------------------------
 
 # number of soil layer
 Numhz=1
 
 # for each dsrate, update input file's (Record C8)
 DSRATE=input_list[Ite,"DSRATE"]
 row_hz=2265
 for (i in 1:Numhz){
   row_t=row_hz+(i-1)
   DSRATE_list <- unlist(strsplit(a[row_t],","))
   DSRATE_list[2]<-DSRATE
   a[row_t]=paste(DSRATE_list,collapse=",")
 }

 
 
# ------------------------------------------------------------------------------
# create a przm input (will have one for each of the simulations = 5000)
# ------------------------------------------------------------------------------
  newdir <- paste0(pwcdir,"input/przm/input",Ite)
  print(newdir)
  dir.create(newdir,showWarnings = FALSE) 
  cwd <- getwd() #current directory
  setwd(newdir) 
  
  # copy weather file
  weather_input <- paste(pwcdir_exe, pwc_weather_used, sep="")
  file.copy(weather_input,newdir, recursive = FALSE, 
            copy.mode = TRUE)

  file_path_as_absolute(newdir)
  
  # update weather and output under each input folder
  a[4]=paste(file_path_as_absolute(newdir),"/",pwc_weather_used, sep="")
  a[6]=paste(file_path_as_absolute(newdir),"/","output.zts", sep="")
  
  # copy PRZM.exe
  print(paste(file.exists(przmdir_executable), ": executable file at", przmdir_executable))
  file.copy(przmdir_executable,newdir, recursive = FALSE, 
            copy.mode = TRUE)
  
  # update vvwm -- change to function
  source(paste(pwcdir,"src/04_write_update_vvwm.R",sep = "")) 
    #if warning: "incomplete final line found on vvwmTransfer.txt" --> open file, press <enter> on last line and save file. 

  # copy VVWM.exe
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

# ---------------------------------------------------------------------------
# read in outputs, fill for each simulation
# ---------------------------------------------------------------------------
  
  # ---------------------------- PRZM Output --------------------------------
  df <- read.table(paste(newdir,"/","output",".zts", sep=""), header= FALSE, sep= "",
                   skip = 3, stringsAsFactors = FALSE, row.names=NULL)
  #print(df)
  #print(dim(df))
  #newdf <- df[,3:ncols]
  outputdf[1:nrows_przm,1:ncols_przm,Ite] <- abind(df[1:nrows_przm,1:ncols_przm], along=3)
  #print(outputdf)
  #print(dim(outputdf))
  
  # ---------------------------- PWC Output ---------------------------------
  pwcdf <- read.csv(paste(newdir,"/","output_FOL002_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                    skip = 5, stringsAsFactors = FALSE, row.names=NULL)
  

  pwcoutdf[1:rows_pwc,1:cols_pwc,Ite] <- abind(pwcdf[1:rows_pwc,1:cols_pwc], along=3)
  

  
  # ------------------- Reading Conversion Factor from Output ---------------
  con_almond <- file(paste(newdir,"/","output_FOL002_parent_only_Custom_Parent",".txt", sep=""))
  
  open(con_almond)
  con_fac_line <- read.table(con_almond,skip=15,nrow=1) #16-th line
  con_fac[Ite,]<-as.numeric(con_fac_line%>%select_if(is.numeric))
  print(con_fac)
  close(con_almond)
  
  
  
  setwd(cwd)
}




# ----------------------------------------------------------------------------
# the end
# ----------------------------------------------------------------------------