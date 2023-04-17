rm(list = ls())
library(readxl)
library(dplyr)
nsf_dat <- read_excel(here::here("NSF-Data/gss2021_Code.xlsx"))
# loc_dat <- read.csv(here::here("NSF-Data/Most-Recent-Cohorts-Institution.csv"))
# ins_ID <- loc_dat$UNITID
# ins_long <- loc_dat$LONGITUDE 
# ins_lat <- loc_dat$LATITUDE
# ins_useful <- data.frame("UNITID" = ins_ID, "long" = ins_long, "lat" = ins_lat)
# write.csv(ins_useful, here::here("NSF-Data/institution_location.csv"), row.names = FALSE)

loc_dat <- read.csv(here::here("NSF-Data/institution_location.csv"))
nsf_dat$UNITID <- as.integer(nsf_dat$UNITID)
loc_joined <- left_join(nsf_dat, loc_dat, by = "UNITID")

missing_names <- unique(nsf_dat$Institution_Name[which(is.na(loc_joined$long))])

institution_df <- data.frame(
  institution = c("Woods Hole Oceanographic Institution", 
                  "Uniformed Services University of the Health Sciences", 
                  "Memorial Sloan Kettering Cancer Center", 
                  "Scripps Research Institute, The", 
                  "Cold Spring Harbor Laboratory", 
                  "American Museum of Natural History", 
                  "Cedars-Sinai Medical Center", 
                  "Van Andel Institute", 
                  "California University of Science and Medicine"),
  longitude = c(-70.664795, -77.089956, -73.972214, -117.239222, -73.457980, 
                -73.974187, -118.381428, -85.676720, -117.748333),
  latitude = c(41.526449, 39.003269, 40.765226, 32.880069, 40.887596, 
               40.780788, 34.076206, 42.963367, 34.072000)
)

for (curr_name in missing_names) {
  
  curr_missing_idx <- which(loc_joined$Institution_Name == curr_name)
  curr_long <-institution_df$longitude[which(institution_df$institution == curr_name)]
  curr_lat <-institution_df$latitude[which(institution_df$institution == curr_name)]
  loc_joined$long[curr_missing_idx] <- curr_long
  loc_joined$lat[curr_missing_idx] <- curr_lat
  
}


sum(is.na(loc_dat$long))
