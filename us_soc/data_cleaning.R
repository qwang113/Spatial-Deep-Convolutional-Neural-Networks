library(soilDB)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(xtable)
library(geoR)
library(sp)
library(gstat)
library(cowplot)
library(mvtnorm)
library(MBA)
library(fields)


all_states <- state.abb[-c(2,11)]
soc_all <- fetchRaCA(state = c("AL"))$sample %>% 
  dplyr::select(sample_id, rcapid, soc, soc_sd, soc_measured, sample_top, sample_bottom, texture)

socSite <- fetchRaCA(state = c("AL"))$pedons@site%>% 
  dplyr::select(rcapid, elevation=elev_field, long=x, lat=y, landuse) ## location data

out <- soc_all %>% left_join(socSite, by="rcapid") %>%
  filter(sample_top==0) %>% filter(soc_measured == "measured") ## just taking the top layer of soil

out <- cbind(out, "AL")
colnames(out)[length(colnames(out))] <- "State"

for (i in 2:length(all_states)) {
  
  print(paste("Getting State " ,all_states[i]))
  
  soc_temp <- fetchRaCA(state = all_states[i])$sample %>% 
    dplyr::select(sample_id, rcapid, soc, soc_sd, soc_measured, sample_top, sample_bottom, texture)
  
  socSite_temp <- fetchRaCA(state = all_states[i])$pedons@site%>% 
    dplyr::select(rcapid, elevation=elev_field, long=x, lat=y, landuse) ## location data
  
  out_temp <- soc_temp %>% left_join(socSite_temp, by="rcapid") %>%
    filter(sample_top==0) %>% filter(soc_measured == "measured") ## just taking the top layer of soil
  
  
  out_temp <- cbind(out_temp, all_states[i])
  colnames(out_temp)[length(colnames(out_temp))] <- "State"
  out <- rbind(out, out_temp)
  
  
}

write.csv(out, file = "us_soc1.csv")




