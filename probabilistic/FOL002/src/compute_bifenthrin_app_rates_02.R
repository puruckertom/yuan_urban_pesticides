# ---------------------------------------------------------------
# create *.txt file with 2192 app rates
# ---------------------------------------------------------------




# ---------------------------------------------------------------
# placer county
# ---------------------------------------------------------------
# read in *.csv
placer <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/placer_09-14_with_homeowner_for_loop.csv", header=TRUE)

# check/name variables
nrow(placer)
days <- placer$days


# create output text file containing each day's app rate
sink(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/placer_output.txt", append=TRUE)


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







# ---------------------------------------------------------------
# sacramento county
# ---------------------------------------------------------------


# read in *.csv
sac <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/sacramento_09-14_with_homeowner_for_loop.csv", header=TRUE)

# check/name variables
nrow(sac)
days <- sac$days


# create output text file containing each day's app rate
#placer_output <- 
sink(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/sacramento_output.txt", append=TRUE)


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




# ---------------------------------------------------------------
# the end
# ---------------------------------------------------------------