# ---------------------------------------------------------------------------
# plot deterministic model outputs
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# read in data
# ---------------------------------------------------------------------------

fol002_h2_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_ave_conc_h2.csv", sep=",", 
                           header=TRUE)
fol002_peak_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_peak_conc_h2.csv", sep=",", 
                           header=TRUE)
fol002_benthic_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_ave_conc_benthic.csv", sep=",", 
                           header=TRUE)
fol002_runoff_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol002_runf.csv", sep=",", 
                           header=TRUE)

fol003_h2_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_ave_conc_h2.csv", sep=",", 
                           header=TRUE)
fol003_peak_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_peak_conc_h2.csv", sep=",", 
                             header=TRUE)
fol003_benthic_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_ave_conc_benthic.csv", sep=",", 
                                header=TRUE)
fol003_runoff_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/fol003_runf.csv", sep=",", 
                               header=TRUE)


pgc010_h2_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_ave_conc_h2.csv", sep=",", 
                           header=TRUE)
pgc010_peak_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_peak_conc_h2.csv", sep=",", 
                             header=TRUE)
pgc010_benthic_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_ave_conc_benthic.csv", sep=",", 
                                header=TRUE)
pgc010_runoff_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc010_runf.csv", sep=",", 
                               header=TRUE)

pgc022_h2_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_ave_conc_h2.csv", sep=",", 
                           header=TRUE)
pgc022_peak_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_peak_conc_h2.csv", sep=",", 
                             header=TRUE)
pgc022_benthic_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_ave_conc_benthic.csv", sep=",", 
                                header=TRUE)
pgc022_runoff_df <- read.table("C:/Users/echelsvi/git/yuan_urban_pesticides/deterministic/io/pgc022_runf.csv", sep=",", 
                               header=TRUE)


# ---------------------------------------------------------------------------
# monthly precip and runoff
# ---------------------------------------------------------------------------

# create dataframes for usgs and total precip
# usgs_df <- data.frame(matrix(ncol = 2, nrow = 2192))
# colnames(usgs_df) <- c("date", "usgs")
# usgs_df$date <- as.Date("2008-01-01") + 0:2191
# usgs_df$usgs <- # usgs data
#   
# precip_df <- data.frame(matrix(ncol = 2, nrow = 2192))
# colnames(precip_df) <- c("date", "precip")
# precip_df$date <- as.Date("2008-01-01") + 0:2191
# precip_df$precip <- # total precip data
 

# melt to plot
runoff_merge_1 <- merge(fol002_runoff_df, fol003_runoff_df, by="date")
runoff_merge_2 <- merge(runoff_merge_1, pgc010_runoff_df, by="date")
runoff_merge_3 <- merge(runoff_merge_2, pgc022_runoff_df, by="date")
#runoff_merge_4 <- merge(runoff_merge_3, usgs_df, by="date")
#runoff_merge_5 <- merge(runoff_merge_4, precip_df, by="date")


runoff_melt <- melt(runoff_merge_3, id.var="date")

#not sure about units

# plot
ggplot(runoff_melt, aes(x=date, y=value, col=variable)) +
  geom_line() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "5 months") +
  xlab("Date") +
  ylab("Monthly Precipitation and Runoff (mm)")






# ---------------------------------------------------------------------------
# observed and simulated bifenthrin concentration
# ---------------------------------------------------------------------------

# create dataframe for observed 
# ????


# melt to plot




# plot











# ---------------------------------------------------------------------------
# the end
# ---------------------------------------------------------------------------