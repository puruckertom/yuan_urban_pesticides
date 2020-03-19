# ---------------------------------------------------------------
# create *.txt file with 2192 app rates
# ---------------------------------------------------------------




# ---------------------------------------------------------------
# PGC (Roseville, Placer County sites)
# ---------------------------------------------------------------
# read in *.csv
placer <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/app_rates_09-14_pwc_inputs_pgc.csv", header=TRUE)

# check/name variables
nrow(placer)
days <- placer$days


# create output text file containing each day's app rate
sink(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_pgc.txt", append=TRUE)


# for every day in each month, write a line regarding the application rate variables for przm.inp
#dd,mm,yy, cam, dep, rate, eff, tband, 1cam, 1dep, 1rate, 1eff, 0, 2cam, 2dep, 2rate, 2eff, 0  
#1,1,09,6,2,0.001117063,0.99,0, 1, 4.0, 0, 0, 0, 1, 4.0, 0, 0, 0

for (i in 1:nrow(placer)){
  # call variables
  mm <- (placer[i,1])
  yy <- placer[i,2]
  cam <- 6
  dep <- 2
  rate <- ((placer[i,5])/(placer[i,3]))
  eff <- 0.99
  tband <- 0
  cam1 <- 1
  dep1 <- 4.0
  rate1 <- 0
  eff1 <- 0
  tband2 <- 0
  cam2 <- 1
  dep2 <- 4.0
  rate2 <- 0
  eff2 <- 0
  tband3 <- 0
  
  for (j in 1:days[i]){
    # call variables
    dd <- j
    
    # write line to file
    cat(paste(dd,mm,yy,cam,dep,rate,eff,tband,cam1,dep1,rate1,eff1,tband2,cam2,dep2,rate2,eff2,tband3,"\n", sep=","))
    
  }
}
sink()





# read file back in to apply a 30-day moving average
ma_placer <- read.table(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_pgc.txt")
con_ma_placer <- file("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_pgc.txt")

a_old=readLines(con_ma_placer)
a=readLines(con_ma_placer)
close(con_ma_placer)


# create vector of all app rates
app_vec <- vector() 

for(i in 1:nrow(ma_placer)){
  day_list <- unlist(strsplit(a[i], ",")) #split up that day's list of var
  app_vec[i] <- day_list[6] #insert that day's app into vec
} 


ma_vec <- vector()
app_vec <- as.numeric(app_vec)

# jan 2009 moving average
for(j in 1:31){
  ma_vec[j] <- app_vec[j]
}

# feb 2009 - end 30-day moving average
for(k in 32:length(app_vec)){
  ma_vec[k] <- (app_vec[k-30]+app_vec[k-29]+app_vec[k-28]+app_vec[k-27]+app_vec[k-26]+app_vec[k-25]+app_vec[k-24]+app_vec[k-23]+app_vec[k-22]+
                  app_vec[k-21]+app_vec[k-20]+app_vec[k-19]+app_vec[k-18]+app_vec[k-17]+app_vec[k-16]+app_vec[k-15]+app_vec[k-14]+app_vec[k-13]+
    app_vec[k-12]+app_vec[k-11]+app_vec[k-10]+app_vec[k-9]+app_vec[k-8]+app_vec[k-7]+app_vec[k-6]+app_vec[k-5]+app_vec[k-4]+app_vec[k-3]+
    app_vec[k-2]+app_vec[k-1])/30
}

ma_vec <- as.character(ma_vec)


# insert the app rates back into the file
con2_ma_placer <- file("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_pgc.txt")

b_old=readLines(con2_ma_placer)
b=readLines(con2_ma_placer)
close(con2_ma_placer)

for(l in 1:length(ma_vec)){
  day_list2 <- unlist(strsplit(b[l], ",")) #split up that day's list of var
  day_list2[6] <- ma_vec[l]
  b[l]=paste(day_list2,collapse=",")
} 


# write out file
out_file <- paste("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_with_ma_pgc",".txt", sep="")
file.exists(out_file)
file.create(out_file)
file.exists(out_file)
con_apps <- file(out_file)
writeLines(b, con_apps)
close(con_apps)






# ---------------------------------------------------------------
# sacramento county
# ---------------------------------------------------------------


# read in *.csv
sac <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/app_rates_09-14_pwc_inputs_folsom.csv", header=TRUE)

# check/name variables
nrow(sac)
days <- sac$days


# create output text file containing each day's app rate
#placer_output <- 
sink(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_folsom.txt", append=TRUE)


# for every day in each month, write a line regarding the application rate variables for przm.inp
#dd,mm,yy, cam, dep, rate, eff, tband, 1cam, 1dep, 1rate, 1eff, 0, 2cam, 2dep, 2rate, 2eff, 0  
#1,1,09,6,2,0.001117063,0.99,0, 1, 4.0, 0, 0, 0, 1, 4.0, 0, 0, 0

for (i in 1:nrow(sac)){
  # call variables
  mm <- (sac[i,1])
  yy <- sac[i,2]
  cam <- 6
  dep <- 2
  rate <- ((sac[i,5])/(sac[i,3]))
  eff <- 0.99
  tband <- 0
  cam1 <- 1
  dep1 <- 4.0
  rate1 <- 0
  eff1 <- 0
  tband2 <- 0
  cam2 <- 1
  dep2 <- 4.0
  rate2 <- 0
  eff2 <- 0
  tband3 <- 0
  
  for (j in 1:days[i]){
    # call variables
    dd <- j
    
    # write line to file
    cat(paste(dd,mm,yy,cam,dep,rate,eff,tband,cam1,dep1,rate1,eff1,tband2,cam2,dep2,rate2,eff2,tband3,"\n", sep=","))
    
  }
}
sink()




# read file back in to apply a 30-day moving average
ma_sac <- read.table(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_folsom.txt")
con_ma_sac <- file("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_folsom.txt")

a_old=readLines(con_ma_sac)
a=readLines(con_ma_sac)
close(con_ma_sac)


# create vector of all app rates
app_vec <- vector() 

for(i in 1:nrow(ma_sac)){
  day_list <- unlist(strsplit(a[i], ",")) #split up that day's list of var
  app_vec[i] <- day_list[6] #insert that day's app into vec
} 


ma_vec <- vector()
app_vec <- as.numeric(app_vec)

# jan 2009 moving average
for(j in 1:31){
  ma_vec[j] <- app_vec[j]
}

# feb 2009 - end 30-day moving average
for(k in 32:length(app_vec)){
  ma_vec[k] <- (app_vec[k-30]+app_vec[k-29]+app_vec[k-28]+app_vec[k-27]+app_vec[k-26]+app_vec[k-25]+app_vec[k-24]+app_vec[k-23]+app_vec[k-22]+
                  app_vec[k-21]+app_vec[k-20]+app_vec[k-19]+app_vec[k-18]+app_vec[k-17]+app_vec[k-16]+app_vec[k-15]+app_vec[k-14]+app_vec[k-13]+
                  app_vec[k-12]+app_vec[k-11]+app_vec[k-10]+app_vec[k-9]+app_vec[k-8]+app_vec[k-7]+app_vec[k-6]+app_vec[k-5]+app_vec[k-4]+app_vec[k-3]+
                  app_vec[k-2]+app_vec[k-1])/30
}

ma_vec <- as.character(ma_vec)


# insert the app rates back into the file
con2_ma_sac <- file("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_folsom.txt")

b_old=readLines(con2_ma_sac)
b=readLines(con2_ma_sac)
close(con2_ma_sac)

for(l in 1:length(ma_vec)){
  day_list2 <- unlist(strsplit(b[l], ",")) #split up that day's list of var
  day_list2[6] <- ma_vec[l]
  b[l]=paste(day_list2,collapse=",")
} 


# write out file
out_file <- paste("C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/output_for_pwc_with_ma_folsom",".txt", sep="")
file.exists(out_file)
file.create(out_file)
file.exists(out_file)
con_apps <- file(out_file)
writeLines(b, con_apps)
close(con_apps)



# ---------------------------------------------------------------
# the end
# ---------------------------------------------------------------