# ------------------------------------------------------------------------------
# compute watershed app rates for Roseville sites (PGC010, PGC022)
# ------------------------------------------------------------------------------


# Using Luo,2015 methods

# To calulate urban pesticide use in the Pleasant Grove Creek Watershed:
#
#   [.1787 * PUR(Placer County)] + [.0003 * PUR(Sutter County)]
#
# where PUR(County) is the reported urban bifenthrin uses from the PUR database



# read in files
placer <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/placer/placer_09-14_with_homeowner.csv", 
                   header=T, sep=",")
sutter <- read.csv(file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/sutter/sutter_09-14_with_homeowner.csv", 
                   header=T, sep=",")

# create a null matrix to fill
final_mat <- matrix(data=NA, nrow=72, ncol=6)
final_df <- as.data.frame(final_mat)
colnames(final_df) <- c("month", "year", "bif_kg_placer", "bif_kg_sutter", "bif_kg_pgc", "bif_kgha_pgc")

# fill 
final_df$month <- placer$month
final_df$year <- placer$year
final_df$bif_kg_placer <- placer$bif_kg_with_home
final_df$bif_kg_sutter <- sutter$bif_kg_with_home

# compute PGC watershed applications (kg)
final_df$bif_kg_pgc <- ((.1787 * final_df$bif_kg_placer) + (0.0003 * final_df$bif_kg_sutter))


# compute PGC watershed application rate (kg/ha)
# 6662.25 ha
final_df$bif_kgha_pgc <- final_df$bif_kg_pgc/6662.25

# subset desired cols, write out
final_output <- final_df[, c("month", "year", "bif_kg_pgc", "bif_kgha_pgc")]
write.csv(final_output, file="C:/Users/echelsvi/git/yuan_urban_pesticides/bifenthrin_application_rates/CALPIP/app_rates_09-14_pgc.csv", row.names=F)



# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------