# ------------------------------------------------------------------------------
# compute watershed app rates for Folsom sites (FOL002, FOL003)
# ------------------------------------------------------------------------------


# Using Luo,2015 methods

# To calulate urban pesticide use in the Folsom Watershed:
#
#   [.0839 * PUR(Sacramento County)] + [.0018 * PUR(El Dorado County)] + [.0004 * PUR(Placer County)]
#
# where PUR(County) is the reported urban bifenthrin uses from the PUR database



# read in files
placer <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/placer/placer_09-14_with_homeowner.csv", 
                   header=T, sep=",")
sac <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sacramento/sacramento_09-14_with_homeowner.csv", 
                   header=T, sep=",")
eld <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/eldorado/eldorado_09-14_with_homeowner.csv", 
                header=T, sep=",")

# create a null matrix to fill
final_mat <- matrix(data=NA, nrow=72, ncol=7)
final_df <- as.data.frame(final_mat)
colnames(final_df) <- c("month", "year", "bif_kg_placer", "bif_kg_sac","bif_kg_eld", "bif_kg_folsom", "bif_kgha_folsom")

# fill 
final_df$month <- placer$month
final_df$year <- placer$year
final_df$bif_kg_placer <- placer$bif_kg_with_home
final_df$bif_kg_sac <- sac$bif_kg_with_home
final_df$bif_kg_eld <- eld$bif_kg_with_home

# compute PGC watershed applications (kg)
final_df$bif_kg_folsom <- ((.0839 * final_df$bif_kg_sac) + (.0018 * final_df$bif_kg_eld) + (0.0004 * final_df$bif_kg_placer))


# compute Folsom watershed application rate (kg/ha)
# 8162.82 ha
final_df$bif_kgha_folsom <- final_df$bif_kg_folsom/8162.82

# subset desired cols, write out
final_output <- final_df[, c("month", "year", "bif_kg_folsom", "bif_kgha_folsom")]
write.csv(final_output, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/app_rates_09-14_folsom.csv", row.names=F)



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------