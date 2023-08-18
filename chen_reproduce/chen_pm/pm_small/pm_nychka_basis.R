# Adding Covariates
rm(list = ls())
library(FRK)
library(spNNGP)
library(ggplot2)
library(maps)
library(MBA)
library(fields)
library(sp)
library(ncdf4)
library(reticulate)
use_condaenv("tf_gpu")
library(tensorflow)
library(keras)
cov_all <- read.csv(here::here("covariate0605.csv"), header = T)
pm_dat <- read.csv(here::here("pm25_0605.csv"), header = T)

obs_loc <- cbind(pm_dat$Longitude, pm_dat$Latitude)
cov_loc <- cbind(cov_all$long, cov_all$lat)
pair_dist <- spDists(obs_loc, cov_loc)

cov_idx <- apply(pair_dist, 1, which.min)
obs_pm <- pm_dat$PM25
obs_cov <- as.data.frame(cov_all[cov_idx, -1])

agg_df <- aggregate(obs_pm ~ obs_cov$long + obs_cov$lat, FUN = mean)
long <- agg_df$`obs_cov$long`
lat <- agg_df$`obs_cov$lat`
pm <- agg_df$obs_pm
dat_pm <- as.data.frame(cbind(long, lat, pm))
coordinates(dat_pm) <- ~ long + lat

cov_final <- matrix(NA, nrow = nrow(dat_pm), ncol = 6)

for (i in 1:nrow(dat_pm)) {
  cov_final[i,] <- as.matrix(cov_all[which(cov_all$long == long[i] & cov_all$lat == lat[i]),(4:9)])
}

mmscale <- function(x)
{
  return( (x-min(x))/(max(x)-min(x)))
}

cov_final <- cbind( long, lat, apply(cov_final, 2, mmscale))


gridbasis1 <- auto_basis(mainfold = plane(), data = dat_pm, nres = 1, type = "Gaussian", regular = 1)
gridbasis2 <- auto_basis(mainfold = plane(), data = dat_pm, nres = 2, type = "Gaussian", regular = 1)
gridbasis3 <- auto_basis(mainfold = plane(), data = dat_pm, nres = 3, type = "Gaussian", regular = 1)

show_basis(gridbasis3) + 
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude")

loc_1 <- matrix(sapply(sapply(gridbasis1@pars, "[[", "loc"), unlist),ncol = 2, byrow = T)
loc_2 <- matrix(sapply(sapply(gridbasis2@pars, "[[", "loc"), unlist),ncol = 2, byrow = T)
loc_3 <- matrix(sapply(sapply(gridbasis3@pars, "[[", "loc"), unlist),ncol = 2, byrow = T)


nychka_fun <- function(center_coords , obs_coords, theta){
  
  d <- sqrt( (center_coords[1] - obs_coords[1])^2 + (center_coords[2]- obs_coords[2])^2)/theta
  
  if(d > 1 )
  {
    out <- 0
  }else{
    out <- (1-d)^6 * (35*d^2 + 18*d + 3)/3
  }
  
  return(out)
}

loc_3 <- loc_3[-(1:nrow(loc_2)),]
depth <- 3
shape_row <- length(table(gridbasis3@df[which(gridbasis3@df$res == depth) , 2 ]))
shape_col <- length(table(gridbasis3@df[which(gridbasis3@df$res == depth) , 1 ]))

basis_nychka <- function(center, observation){
  
  
  theta <- 2.5 * sqrt(
    (sort(unique(center[,1]))[1] - sort(unique(center[,1]))[2])^2 + 
    (sort(unique(center[,2]))[1] - sort(unique(center[,2]))[2])^2
                )   
  
  basis_array <- matrix(NA,  nrow = nrow(observation), ncol = nrow(center)) 
  
  for (i in 1:nrow(observation)) {
    for (j in 1:nrow(center)) {
      basis_array[i,j] <- nychka_fun(center_coords = center[j,], obs_coords = observation[i,], theta = theta)
    }
  }
  
  return(basis_array)
}
  



basis_mat <- basis_nychka(center = loc_3, observation = coordinates(dat_pm))
# useless_index <- which(colSums(basis_mat) == 0)
# basis_mat <- basis_mat[,-useless_index]
# basis_arr <- array(NA, dim = c(nrow(dat_pm), shape_row, shape_col))
# 
# for (i in 1:nrow(dat_pm)) {
#   basis_arr[i,,] <- matrix(basis_use[i,], nrow = shape_row, ncol = shape_col, byrow = T)
# }


set.seed(0)
train_index <- sample(1:nrow(dat_pm), 500, replace = FALSE)


basis_tr <- basis_mat[train_index,]
basis_te <- basis_mat[-train_index,]
cov_final <- apply(cov_final, 2, mmscale)
cov_tr <- cov_final[train_index,]
cov_te <- cov_final[-train_index,]


 x_tr <- as.matrix(cbind(array_reshape(basis_tr, c(nrow(basis_tr), shape_row*shape_col)), cov_tr)) # So we want to reshape each observation from a picture to a vector
 x_te <- as.matrix(cbind(array_reshape(basis_te, c(nrow(basis_te), shape_row*shape_col)), cov_te))  # Same as prervious step

 x_tr <- cbind(basis_tr, cov_tr)
 x_te <- cbind(basis_te, cov_te)
# 
# x_tr <- basis_tr
# x_te <- basis_te

pm_tr <- pm[train_index]
pm_te <- pm[-train_index]


model_dk <- keras_model_sequential() 

model_dk %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(ncol(x_tr))) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  # layer_dense(units = 256, activation = 'relu') %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')

# Compile the model

model_dk %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mse")
)

mod_train_dk <- model_dk %>%
  fit(x = x_tr, y = pm_tr, epochs = 30, batch_size = 128, validation_split = 0.1)

loss_dk_cov <- model_dk %>%
  evaluate(x_te, pm_te)


