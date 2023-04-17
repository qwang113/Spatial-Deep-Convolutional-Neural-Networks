library(spNNGP)
library(ggplot2)
library(maps)
library(MBA)
library(fields)
library(foreach)


# Read the data
pm_large_all <- read.csv(here::here("annual_conc_by_monitor_2022.csv"))
pm_large_all <- pm_large_all[which(pm_large_all$Parameter.Name == "PM2.5 - Local Conditions"),]
point_aba <- unique(c(which(pm_large_all$Longitude <= -130), which(pm_large_all$Latitude <=20 ) ))
pm_large_all <- pm_large_all[-point_aba,]


long <- pm_large_all$Longitude + rnorm( length(pm_large_all$Longitude), 0 , sd = 0.001)
lat <- pm_large_all$Latitude + rnorm( length(pm_large_all$Latitude), 0 , sd = 0.001)
pm_filt <- pm_large_all$Arithmetic.Mean

pm_large <- as.data.frame(cbind(long, lat, pm_filt))
set.seed(0)
cv_index_all <- sample(1:5, size = nrow(pm_large_all), replace = TRUE)
index_num <- table(cv_index_all)

pm_large_spnn_cv <- function(curr_index)
{
  curr_index <- which(cv_index_all == curr_index)
  pm_filt_tr <- pm_filt[-curr_index]
  long_tr <- long[-curr_index]
  lat_tr <- lat[-curr_index]
  
  pm_filt_te <- pm_filt[curr_index]
  long_te <- long[curr_index]
  lat_te <- lat[curr_index]
  
  
  
  spnn_mod_15 <- spNNGP(pm_filt_tr ~ 1, coords = cbind(long_tr,lat_tr), cov.model = "exponential",
                       priors = list("sigma.sq.IG" = c(0.001, 0.001),"tau.sq.IG" = c(0.001,0.001), "phi.Unif" = c(0.1,3)),
                       starting = list("sigma.sq" = 100, "tau.sq" = 10, "phi" = 0.5),
                       tuning = list( "sigma.sq" = 0.1, "tau.sq" = 0.1, "phi" = 0.1)
                       , n.neighbors = 15, n.samples = 10000 )
  cv_curr_pred <- predict(spnn_mod_15, X.0 = matrix(1, nrow = length(long_te), ncol = 1), coords.0 = cbind(long_te, lat_te))$p.y.0
  
  curr_mse <- (apply(cv_curr_pred, 1, mean) - pm_filt_te)^2
  return(curr_mse)
}


cv_mse_nngp <- foreach(i = 1:5, .combine = "c") %dopar% pm_large_spnn_cv (i)

# 
# mse_all  <- data.frame(dk = cv_mse_dk, ck = cv_mse_ck, nngp = cv_mse_nngp)
# 
# 
# plot(cv_mse_nngp, type = 'p', ylim = c(0,1))
# lines(cv_mse_ck, col = "red")
# 
# write.csv(mse_all, "pm_25_large_mse_all")
