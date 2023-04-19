library(spNNGP)
library(ggplot2)
library(maps)
library(MBA)
library(fields)
library(foreach)


# Read the data
acc_loc <- read.table(here::here("urban/urbanGB.txt"), sep = ",")
acc_num <- read.table(here::here("urban/urbanGB.labels.txt"))$V1


long <- acc_loc[,1]
lat <- acc_loc[,2]

set.seed(0)
cv_index_all <- sample(1:5, size = nrow(acc_loc), replace = TRUE)
index_num <- table(cv_index_all)

ra_large_spnn_cv <- function(curr_index)
{
  train_index <- which(cv_index_all == curr_index)
  ra_tr <- acc_num[-train_index]
  long_tr <- long[-train_index]
  lat_tr <- lat[-train_index]
  
  ra_te <- acc_num[train_index]
  long_te <- long[train_index]
  lat_te <- lat[train_index]
  
  
  
  spnn_mod_15 <- spNNGP(ra_tr ~ 1, coords = cbind(long_tr,lat_tr), cov.model = "exponential",
                       priors = list("sigma.sq.IG" = c(0.001, 0.001),"tau.sq.IG" = c(0.001,0.001), "phi.Unif" = c(0.1,3)),
                       starting = list("sigma.sq" = 100, "tau.sq" = 10, "phi" = 0.5),
                       tuning = list( "sigma.sq" = 0.1, "tau.sq" = 0.1, "phi" = 0.1)
                       , n.neighbors = 15, n.samples = 10000 )
  cv_curr_pred <- predict(spnn_mod_15, X.0 = matrix(1, nrow = length(long_te), ncol = 1), coords.0 = cbind(long_te, lat_te))$p.y.0
  
  curr_mse <- (apply(cv_curr_pred, 1, mean) - ra_te)^2
  return(curr_mse)
}


cv_mse_nngp <- foreach(i = 1:5, .combine = "c") %dopar% ra_large_spnn_cv (i)

# 
# mse_all  <- data.frame(dk = cv_mse_dk, ck = cv_mse_ck, nngp = cv_mse_nngp)
# 
# 
# plot(cv_mse_nngp, type = 'p', ylim = c(0,1))
# lines(cv_mse_ck, col = "red")
# 
# write.csv(mse_all, "pm_25_large_mse_all")
