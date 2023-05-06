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
library(geoR)
use_condaenv("tf_gpu")
library(tensorflow)
library(keras)


pm_dat <- read.csv(here::here("pm25_0605.csv"), header = T)
cov_dat <- read.csv(here::here("covariate0605.csv"), header = T)

pm_loc <- cbind(pm_dat$Longitude, pm_dat$Latitude)
cov_loc <- cbind(cov_dat$long, cov_dat$lat)

ggplot() +
  geom_point(aes(x = cov_loc[,1], y = cov_loc[,2]), size = 0.5)


obs_grid_dist <- spDists(pm_loc, cov_loc)

cov_idx <- apply(obs_grid_dist, 1, which.min)

pm_idxed <- cbind(pm_dat, cov_idx)
pm_avg <- aggregate(PM25 ~ cov_idx, data = pm_dat, FUN = mean)

pm_cov <- cbind(pm_avg, cov_dat[pm_avg[,1],])[,-c(1,3)]

long <- pm_cov$long
lat <- pm_cov$lat
pm <- pm_cov$PM25
cov_all <- cbind(long, lat,pm_cov[,4:9])

min_max_scale <- function(x)
{
  low <- range(x)[1]
  high <- range(x)[2]
  
  out <- (x - low)/(high - low)
  return(out)
  
}

scaled_cov <- apply(cov_all, 2, min_max_scale)
scaled_long <- scaled_cov[,1]
scaled_lat <- scaled_cov[,2]
scaled_coords <- cbind(scaled_long, scaled_lat)


nychka_fun <- function(spdist, theta){
  
  d <- spdist/theta
  
  out <- matrix(NA, nrow = nrow(spdist), ncol = ncol(spdist))
  
  out[which(d > 1)] <- 0
  non_0 <- which(d<=1)
  out[non_0] <- (1-d[non_0])^6 * (35*d[non_0]^2 + 18*d[non_0] + 3)/3
  return(out)
}

basis_1 <- expand.grid(seq(from = 0, to = 1, length.out = 10),seq(from = 0, to = 1, length.out = 10))  
basis_2 <- expand.grid(seq(from = 0, to = 1, length.out = 19),seq(from = 0, to = 1, length.out = 19))
basis_3 <- expand.grid(seq(from = 0, to = 1, length.out = 37),seq(from = 0, to = 1, length.out = 37))
basis_4 <- expand.grid(seq(from = 0, to = 1, length.out = 73),seq(from = 0, to = 1, length.out = 73))

basis_dist_1 <- spDists(as.matrix(scaled_coords), as.matrix(basis_1))
basis_dist_2 <- spDists(as.matrix(scaled_coords), as.matrix(basis_2))
basis_dist_3 <- spDists(as.matrix(scaled_coords), as.matrix(basis_3))
basis_dist_4 <- spDists(as.matrix(scaled_coords), as.matrix(basis_4))

theta_1 <- 2.5* diff(seq(from = 0, to = 1, length.out = 10))[1]
theta_2 <- 2.5* diff(seq(from = 0, to = 1, length.out = 19))[1]
theta_3 <- 2.5* diff(seq(from = 0, to = 1, length.out = 37))[1]
theta_4 <- 2.5* diff(seq(from = 0, to = 1, length.out = 73))[1]

basis_fun_1 <- nychka_fun(basis_dist_1, theta = theta_1)
basis_fun_2 <- nychka_fun(basis_dist_2, theta = theta_2)
basis_fun_3 <- nychka_fun(basis_dist_3, theta = theta_3)
basis_fun_4 <- nychka_fun(basis_dist_4, theta = theta_4)



set.seed(0)
train_index_all <- sample(1:10, length(pm), replace = T)
loss_dnn <- rep(NA, 10)
loss_dk <- rep(NA, 10)
loss_ck <- rep(NA, 10)
loss_krig <- rep(NA, 10)

pred_dnn <- rep(NA, length(pm))
pred_dk <- rep(NA, length(pm))
pred_ck <- rep(NA, length(pm))
pred_krig <- rep(NA, length(pm))



shape_col_1 <- shape_row_1 <- 10
basis_arr_1 <- array(NA, dim = c(length(pm), shape_row_1, shape_col_1))

for (i in 1:length(pm)) {
  basis_arr_1[i,,] <- matrix(basis_fun_1[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = TRUE)
}




# Second resolution
shape_col_2 <- shape_row_2 <- 19
basis_arr_2 <- array(NA, dim = c(length(pm), shape_row_2, shape_col_2))
for (i in 1:length(pm)) {
  basis_arr_2[i,,] <- matrix(basis_fun_2[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = TRUE)
}

# Third resolution
shape_col_3 <- shape_row_3 <- 37
basis_arr_3 <- array(NA, dim = c(length(pm), shape_row_3, shape_col_3))
for (i in 1:length(pm)) {
  basis_arr_3[i,,] <- matrix(basis_fun_3[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = TRUE)
}

# Fourth resolution
shape_col_4 <- shape_row_4 <- 73
basis_arr_4 <- array(NA, dim = c(length(pm), shape_row_4, shape_col_4))
for (i in 1:length(pm)) {
  basis_arr_4[i,,] <- matrix(basis_fun_4[i,], nrow = shape_row_4, ncol = shape_col_4, byrow = TRUE)
}

for (curr_index in 1:10) {
  
  train_index <- which(train_index_all != curr_index)
  
  basis_tr_1 <- array_reshape(basis_arr_1[train_index,,], c(length(train_index), shape_row_1, shape_col_1, 1))
  basis_tr_2 <- array_reshape(basis_arr_2[train_index,,], c(length(train_index), shape_row_2, shape_col_2, 1))
  basis_tr_3 <- array_reshape(basis_arr_3[train_index,,], c(length(train_index), shape_row_3, shape_col_3, 1))
  basis_tr_4 <- array_reshape(basis_arr_4[train_index,,], c(length(train_index), shape_row_4, shape_col_4, 1))
  
  basis_te_1 <- array_reshape(basis_arr_1[-train_index,,], c(length(pm) - length(train_index), shape_row_1, shape_col_1, 1))
  basis_te_2 <- array_reshape(basis_arr_2[-train_index,,], c(length(pm) - length(train_index), shape_row_2, shape_col_2, 1))
  basis_te_3 <- array_reshape(basis_arr_3[-train_index,,], c(length(pm) - length(train_index), shape_row_3, shape_col_3, 1))
  basis_te_4 <- array_reshape(basis_arr_4[-train_index,,], c(length(pm) - length(train_index), shape_row_4, shape_col_4, 1))
  
  cov_tr <- scaled_cov[train_index,]
  cov_te <- scaled_cov[-train_index,]
  
  drop = 0.1
  # We need three convolutional input model and adding covariates.
  input_basis_1 <- layer_input(shape = c(shape_row_1, shape_col_1, 1))
  input_basis_2 <- layer_input(shape = c(shape_row_2, shape_col_2, 1))
  input_basis_3 <- layer_input(shape = c(shape_row_3, shape_col_3, 1))
  input_basis_4 <- layer_input(shape = c(shape_row_4, shape_col_4, 1))
  input_cov <- layer_input(shape = ncol(cov_tr))
  
  
  resolution_1_conv <- input_basis_1 %>%
    layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu') %>%
    
    # layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten()
  
  resolution_2_conv <- input_basis_2 %>%
    layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu') %>%
    # layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten()
  
  resolution_3_conv <- input_basis_3 %>%
    layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu') %>%
    # layer_max_pooling_2d(pool_size = c(3,3)) %>%
    layer_flatten() 
  
  resolution_4_conv <- input_basis_4 %>%
    layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu') %>%
    # layer_max_pooling_2d(pool_size = c(4,4)) %>%
    layer_flatten() 
  
  cov_model <- input_cov 
  
  all_model <- layer_concatenate(list(resolution_1_conv, resolution_2_conv, resolution_3_conv, resolution_4_conv, cov_model))
  # all_model <- layer_concatenate(list(resolution_1_conv, resolution_2_conv, resolution_3_conv, cov_model))
  
  
  output_layer <- all_model %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 100, activation = 'relu') %>%  
    layer_dense(units = 100, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  
  model_ck <- keras_model(inputs = list(input_basis_1, input_basis_2, input_basis_3,input_basis_4, input_cov), outputs = output_layer)
  
  model_ck <- 
    model_ck %>% compile(
      optimizer = optimizer_adam(),
      loss = 'mse',
      metrics = c('mse')
    )
  model_checkpoint <- callback_model_checkpoint(
    filepath = "C:/Users/10616/Desktop/temp/best_weights.h5",
    save_best_only = TRUE,
    monitor = "val_loss",
    mode = "min",
    verbose = 1
  )
  
  mod_train_ck <- model_ck %>% fit(
    x = list(basis_tr_1, basis_tr_2, basis_tr_3,basis_tr_4, cov_tr),
    y = pm[train_index],
    epochs=1000,
    batch_size=16,
    validation_data=list(list(basis_te_1,basis_te_2,basis_te_3,basis_te_4,cov_te), pm[-train_index]),
    callbacks = model_checkpoint
  )
  
  model_ck %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  loss_ck[curr_index] <- evaluate(model_ck, list(basis_te_1,basis_te_2,basis_te_3,basis_te_4,cov_te), pm[-train_index])[2]
  pred_ck[-train_index] <- predict(model_ck, list(basis_te_1,basis_te_2,basis_te_3,basis_te_4,cov_te))
  
}
