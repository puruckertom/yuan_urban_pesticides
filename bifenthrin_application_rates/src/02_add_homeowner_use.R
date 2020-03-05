# ------------------------------------------------------------------------------
# add in homeowner usage
# ------------------------------------------------------------------------------

# read in files
placer <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/placer/placer_09-14.csv", header=T)
sac <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sacramento/sacramento_09-14.csv", header=T)
sutter <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sutter/sutter_09-14.csv", header=T)
eld <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/eldorado/eldorado_09-14.csv", header=T)

# make new df
placer_2 <- placer
sac_2 <- sac
sutter_2 <- sutter
eld_2 <- eld 


# compute homeowner rates (estimated as 25% of professional use (Williams, 2010))
placer_2$bif_kg_with_home <- placer$bif_kg + (placer$bif_kg*.25)

sac_2$bif_kg_with_home <- sac$bif_kg + (sac$bif_kg*.25)

sutter_2$bif_kg_with_home <- sutter$bif_kg + (sutter$bif_kg*.25)

eld_2$bif_kg_with_home <- eld$bif_kg + (eld$bif_kg*.25)

# write out files
write.csv(placer_2, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/placer/placer_09-14_with_homeowner.csv", row.names=F)
write.csv(sac_2, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sacramento/sacramento_09-14_with_homeowner.csv", row.names=F)
write.csv(sutter_2, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sutter/sutter_09-14_with_homeowner.csv", row.names=F)
write.csv(eld_2, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/eldorado/eldorado_09-14_with_homeowner.csv", row.names=F)


# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------