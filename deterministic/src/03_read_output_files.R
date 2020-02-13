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
  determdir <- "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic"
}
print(paste("Root directory location: ", determdir, sep=""))


# subdirectories
determdir_input <- paste(determdir, "input/", sep = "/")
determdir_output <- paste(determdir, "output/", sep = "/")
determdir_fig <- paste(determdir, "figures/", sep = "/")




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
pwc_fol002 <- read.csv(paste(determdir_input,"FOL002/outputs/output_FOL002_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                  skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_fol002) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
fol002_h2_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_h2_df) <- c("date", "h2_fol002")
fol002_h2_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_h2_df$h2_fol002 <- (pwc_fol002[,2]*1000000)#in ug/ml
write.table(fol002_h2_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_ave_conc_h2.csv",
          sep=",", row.names=FALSE, col.names=TRUE)

# subset pwc Peak.Conc.H2O*1000000
fol002_peak_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_peak_df) <- c("date", "peak_fol002")
fol002_peak_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_peak_df$peak_fol002 <- (pwc_fol002[,4]*1000000)#in ug/ml
write.table(fol002_peak_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_peak_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset benthic concentration
fol002_benthic_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_benthic_df) <- c("date", "benthic_fol002")
fol002_benthic_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_benthic_df$benthic_fol002 <- (pwc_fol002[,3]*1000000)#in ug/ml
write.table(fol002_benthic_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_ave_conc_benth.csv",
            sep=",", row.names=FALSE, col.names=TRUE)



# read przm output file
przm_fol002 <- read.table(paste(determdir_input,"FOL002/outputs/output",".zts", sep=""), header= FALSE, sep= "",
                 skip = 3, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(przm_fol002) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")

# subset przm runoff 
fol002_runoff_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol002)))
colnames(fol002_runoff_df) <- c("date", "runoff_fol002")
fol002_runoff_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol002)-1)
fol002_runoff_df$runoff_fol002 <- (przm_fol002[,4]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
write.table(fol002_runoff_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_runf.csv",
            sep=",", row.names=FALSE, col.names=TRUE)




# ---------------------------------------------------------------------------
# site: FOL003
# --------------------------------------------------------------------------- 

# read pwc output file
pwc_fol003 <- read.csv(paste(determdir_input,"FOL003/outputs/output_FOL003_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                       skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_fol003) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
fol003_h2_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol003)))
colnames(fol003_h2_df) <- c("date", "h2_fol003")
fol003_h2_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol003)-1)
fol003_h2_df$h2_fol003 <- (pwc_fol003[,2]*1000000)#in ug/ml
write.table(fol003_h2_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_ave_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset pwc Peak.Conc.H2O*1000000
fol003_peak_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol003)))
colnames(fol003_peak_df) <- c("date", "peak_fol003")
fol003_peak_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol003)-1)
fol003_peak_df$peak_fol003 <- (pwc_fol003[,4]*1000000)#in ug/ml
write.table(fol003_peak_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_peak_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset benthic concentration
fol003_benthic_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol003)))
colnames(fol003_benthic_df) <- c("date", "benthic_fol003")
fol003_benthic_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol003)-1)
fol003_benthic_df$benthic_fol003 <- (pwc_fol003[,3]*1000000)#in ug/ml
write.table(fol003_benthic_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_ave_conc_benth.csv",
            sep=",", row.names=FALSE, col.names=TRUE)


# read przm output file
przm_fol003 <- read.table(paste(determdir_input,"FOL003/outputs/output",".zts", sep=""), header= FALSE, sep= "",
                          skip = 3, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(przm_fol003) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")

# subset przm runoff 
fol003_runoff_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_fol003)))
colnames(fol003_runoff_df) <- c("date", "runoff_fol003")
fol003_runoff_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_fol003)-1)
fol003_runoff_df$runoff_fol003 <- (przm_fol003[,4]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
write.table(fol003_runoff_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_runf.csv",
            sep=",", row.names=FALSE, col.names=TRUE)




# ---------------------------------------------------------------------------
# site: PGC010
# --------------------------------------------------------------------------- 

# read pwc output file
pwc_pgc010 <- read.csv(paste(determdir_input,"PGC010/outputs/output_PGC010_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                       skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_pgc010) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
pgc010_h2_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc010)))
colnames(pgc010_h2_df) <- c("date", "h2_pgc010")
pgc010_h2_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc010)-1)
pgc010_h2_df$h2_pgc010 <- (pwc_pgc010[,2]*1000000)#in ug/ml
write.table(pgc010_h2_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_ave_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset pwc Peak.Conc.H2O*1000000
pgc010_peak_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc010)))
colnames(pgc010_peak_df) <- c("date", "peak_pgc010")
pgc010_peak_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc010)-1)
pgc010_peak_df$peak_pgc010 <- (pwc_pgc010[,4]*1000000)#in ug/ml
write.table(pgc010_peak_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_peak_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset benthic concentration
pgc010_benthic_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc010)))
colnames(pgc010_benthic_df) <- c("date", "benthic_pgc010")
pgc010_benthic_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc010)-1)
pgc010_benthic_df$benthic_pgc010 <- (pwc_pgc010[,3]*1000000)#in ug/ml
write.table(pgc010_benthic_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_ave_conc_benth.csv",
            sep=",", row.names=FALSE, col.names=TRUE)



# read przm output file
przm_pgc010 <- read.table(paste(determdir_input,"PGC010/outputs/output",".zts", sep=""), header= FALSE, sep= "",
                          skip = 3, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(przm_pgc010) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")

# subset przm runoff 
pgc010_runoff_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc010)))
colnames(pgc010_runoff_df) <- c("date", "runoff_pgc010")
pgc010_runoff_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc010)-1)
pgc010_runoff_df$runoff_pgc010 <- (przm_pgc010[,4]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
write.table(pgc010_runoff_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_runf.csv",
            sep=",", row.names=FALSE, col.names=TRUE)



# ---------------------------------------------------------------------------
# site: PGC022
# --------------------------------------------------------------------------- 

# read pwc output file
pwc_pgc022 <- read.csv(paste(determdir_input,"PGC022/outputs/output_PGC022_parent_only_Custom_Parent_daily",".csv", sep=""), header= FALSE, sep= ",",
                       skip = 5, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(pwc_pgc022) <- c("Depth(m)","Ave.Conc.H20","Ave.Conc.benth","Peak.Conc.H20")

# subset pwc Ave.Conc.H2O*1000000
pgc022_h2_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc022)))
colnames(pgc022_h2_df) <- c("date", "h2_pgc022")
pgc022_h2_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc022)-1)
pgc022_h2_df$h2_pgc022 <- (pwc_pgc022[,2]*1000000)#in ug/ml
write.table(pgc022_h2_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_ave_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset pwc Peak.Conc.H2O*1000000
pgc022_peak_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc022)))
colnames(pgc022_peak_df) <- c("date", "peak_pgc022")
pgc022_peak_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc022)-1)
pgc022_peak_df$peak_pgc022 <- (pwc_pgc022[,4]*1000000)#in ug/ml
write.table(pgc022_peak_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_peak_conc_h2.csv",
            sep=",", row.names=FALSE, col.names=TRUE)

# subset benthic concentration
pgc022_benthic_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc022)))
colnames(pgc022_benthic_df) <- c("date", "benthic_pgc022")
pgc022_benthic_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc022)-1)
pgc022_benthic_df$benthic_pgc022 <- (pwc_pgc022[,3]*1000000)#in ug/ml
write.table(pgc022_benthic_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_ave_conc_benth.csv",
            sep=",", row.names=FALSE, col.names=TRUE)



# read przm output file
przm_pgc022 <- read.table(paste(determdir_input,"PGC022/outputs/output",".zts", sep=""), header= FALSE, sep= "",
                          skip = 3, stringsAsFactors = FALSE, row.names=NULL)

# update colnames
colnames(przm_pgc022) <- c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")

# subset przm runoff 
pgc022_runoff_df <- data.frame(matrix(ncol = 2, nrow = nrow(pwc_pgc022)))
colnames(pgc022_runoff_df) <- c("date", "runoff_pgc022")
pgc022_runoff_df$date <- as.Date("2008-01-01") + 0:(nrow(pwc_pgc022)-1)
pgc022_runoff_df$runoff_pgc022 <- (przm_pgc022[,4]) # YYYY MM DD RUNF0 ESLS0 RFLX1 EFLX1 DCON1 INFL0
write.table(pgc022_runoff_df, "C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_runf.csv",
            sep=",", row.names=FALSE, col.names=TRUE)



# ---------------------------------------------------------------------------
# the end
# ---------------------------------------------------------------------------