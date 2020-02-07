# ---------------------------------------------------------------------
# run, produce VVWM output
# ---------------------------------------------------------------------


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



# ---------------------------------------------------------------------
# read in the VVWM output dummy file
# ---------------------------------------------------------------------
con <- file(paste(pwcdir, "input/vvwm/vvwmTransfer",".txt",sep=""))
l_old=readLines(con)
l=readLines(con)
close(con)

# ---------------------------------------------------------------------
# for 1:Nsims, update the VVWM dummy file
# ---------------------------------------------------------------------


# ---------------------- kd -------------------------------------------

# round each kd to 1 decimals
kd=round(input_list[Ite,"kd"],1)

# update the dummy file's kd value with this rounded value
kd_list <- unlist(strsplit(l[5],","))
kd_list[1]<-kd
l[5]=paste(kd_list,collapse=",")

# -------------------- aer_aq -----------------------------------------

# round each aer_aq to 2 decimals
aer_aq=round(input_list[Ite,"aer_aq"],2)

# update the dummy file's aer_aq with this rounded value
aer_aq_list <- unlist(strsplit(l[6],","))
aer_aq_list[1]<-aer_aq
l[6]=paste(aer_aq_list,collapse=",")


# ------------------------ temp_ref_aer -------------------------------

# round each to 0 decimals
temp_ref_aer=round(input_list[Ite,"temp_ref_aer"],0)

# update the dummy file's temp_ref_aq with this rounded value
temp_ref_aer_list <- unlist(strsplit(l[7],","))
temp_ref_aer_list[1]<-temp_ref_aer
l[7]=paste(temp_ref_aer_list,collapse=",")


# ------------------ anae_aq ------------------------------------------

# round each to 2 decimals
anae_aq=round(input_list[Ite,"anae_aq"],2)

# update the dummy file's anaq_aq with this rounded value
anae_aq_list <- unlist(strsplit(l[8],","))
anae_aq_list[1]<-anae_aq
l[8]=paste(anae_aq_list,collapse=",")


# --------------------- temp_ref_anae ---------------------------------

# round each to 0 decimals
temp_ref_anae=round(input_list[Ite,"temp_ref_anae"],0)

# update the dummy file's temp_ref_anae with this rounded value
temp_ref_anae_list <- unlist(strsplit(l[9],","))
temp_ref_anae_list[1]<-temp_ref_anae
l[9]=paste(temp_ref_anae_list,collapse=",")


# ---------------------- photolysis -----------------------------------

# round each to 2 decimals
photo=round(input_list[Ite,"photo"],2)

# update the dummy file's photo with this rounded value
photo_list <- unlist(strsplit(l[10],","))
photo_list[1]<-photo
l[10]=paste(photo_list,collapse=",")


# ----------------------- reference latitude --------------------------

# round to 0 decimals
RFLAT=round(input_list[Ite,"RFLAT"],0)

# update the dummy file's rflat with this rounded value
RFLAT_list <- unlist(strsplit(l[11],","))
RFLAT_list[1]<-RFLAT
l[11]=paste(RFLAT_list,collapse=",")


# ------------------------ hydrolysis ---------------------------------

# round to 1 decimal
hydro=round(input_list[Ite,"hydro"],1)

# update the dummy file's hydro with this rounded value
hydro_list <- unlist(strsplit(l[12],","))
hydro_list[1]<-hydro
l[12]=paste(hydro_list,collapse=",")


# ------------------------- solubility mg/L ---------------------------

# round to 2 decimals
SOL=round(input_list[Ite,"SOL"],2)

# update the dummy file's sol with this rounded value
SOL_list <- unlist(strsplit(l[18],","))
SOL_list[1]<-SOL
l[18]=paste(SOL_list,collapse=",")


# --------------------- benthic depth ---------------------------------

# round to 2 decimals
benthic_depth=round(input_list[Ite,"benthic_depth"],2)

# update the dummy file's benthic_depth with this rounded value
l[41]=paste(benthic_depth)


# ----------------------- benthic porosity ----------------------------

# round to 2 decimals
porosity=round(input_list[Ite,"porosity"],2)

# update the dummy file's porosity with this rounded value
l[42]=paste(porosity)


# -------------------- bulk density -----------------------------------

# round to 2
bulk_density=round(input_list[Ite,"bulk_density"],2)

# update the dummy file's bulk density
l[43]=paste(bulk_density)


# ---------------------- froc2 (benthic foc) --------------------------

# round to 2
FROC2=round(input_list[Ite,"FROC2"],3)

# update the dummy file
FROC2_a<-FROC2
l[44]=paste(FROC2_a)


# ------------------------ doc2 (benthic doc) -------------------------

# round to 2
DOC2=round(input_list[Ite,"DOC2"],2)

# update the dummy file
DOC2_a<-DOC2
l[45]=paste(DOC2_a)


# ------------------------ bnmas (benthic biomass) ---------------------

# round to 3
BNMAS=round(input_list[Ite,"BNMAS"],3)

# update the dummy file
BNMAS_a<-BNMAS
l[46]=paste(BNMAS_a)


# --------------------- sused (water columnSS mg/l) --------------------

# round to 3
SUSED=round(input_list[Ite,"SUSED"],3)

# update the dummy file
SUSED_a<-SUSED
l[48]=paste(SUSED_a)


# ------------------------- chl (chlorophyll) --------------------------

# round to 3
CHL=round(input_list[Ite,"CHL"],3)

# update the dummy file
CHL_a<-CHL
l[49]=paste(CHL_a)


# -------------------- froc1 (water column foc) ------------------------

# round to 2
FROC1=round(input_list[Ite,"FROC1"],3)

# update the dummy file
FROC1_a<-FROC1
l[50]=paste(FROC1_a)


# --------------- doc1 (water column doc mg/l) --------------------------

# round to 1
DOC1=round(input_list[Ite,"DOC1"],2)

# update the dummy file
DOC1_a<-DOC1
l[51]=paste(DOC1_a)


# ---------------- plmas (water column biomass mg/l) --------------------

# round to 3
PLMAS=round(input_list[Ite,"PLMAS"],3)

# update the dummy file
PLMAS_a<-PLMAS
l[52]=paste(PLMAS_a)


# #--------------------------- depth_0 ---------------------------------
# depth_0=input_list[Ite,"depth_0"]
# depth_0_a<-depth_0
# l[61]=paste(depth_0_a)


# #--------------------------- depth_max -------------------------------
# depth_max=input_list[Ite,"depth_max"]
# depth_max_a<-depth_max
# l[62]=paste(depth_max_a)

# ------------------------ baseflow -------------------------------------

# round to 2
bf=round(input_list[Ite,"bf"],2)

# update the dummy file
l[65]=paste(bf)


# #---------------------- cropped fraction ------------------------------
# cf=input_list[Ite,"cf"]#cropped fraction
# l[66]=paste(cf)


# ----------------------- file names ------------------------------------
l[1]=paste(file_path_as_absolute(newdir),"/","output", sep="")
l[30]=paste(file_path_as_absolute(newdir),"/",pwc_weather_used, sep="")
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


# -----------------------------------------------------------------------
# write out the file 
# -----------------------------------------------------------------------
vvwm_file <- paste("vvwmTransfer",".txt", sep="")
file.exists(vvwm_file)
file.create(vvwm_file)
file.exists(vvwm_file)
con_vvwm <-file(vvwm_file)
  writeLines(l, con_vvwm)
close(con_vvwm)


# -----------------------------------------------------------------------
# the end
# -----------------------------------------------------------------------