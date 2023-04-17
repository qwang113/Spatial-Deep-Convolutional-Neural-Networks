library(maps)
library(MBA)
library(ggplot2)
us_map <- map_data("state", region=c("alabama", "arizona", "arkansas", "california", "colorado", "connecticut",
                                     "delaware", "florida", "georgia", "idaho", "illinois", "indiana",
                                     "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
                                     "massachusetts", "michigan", "minnesota", "mississippi", "missouri",
                                     "montana", "nebraska", "nevada", "new hampshire", "new jersey",
                                     "new mexico", "new york", "north carolina", "north dakota", "ohio",
                                     "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina",
                                     "south dakota", "tennessee", "texas", "utah", "vermont", "virginia",
                                     "washington", "west virginia", "wisconsin", "wyoming"))

# states = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
#            "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana",
#            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
#            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
#            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
#            "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
#            "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
#            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
#            "West Virginia", "Wisconsin", "Wyoming", "Washington")

unique_obs <- unique(cbind(loc_joined$long, loc_joined$lat))
ggplot() +
   geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
   geom_point(aes(x = unique_obs[,1], y = unique_obs[,2]))

