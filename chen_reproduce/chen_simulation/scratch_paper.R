rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(reticulate)
library(keras)
library(tensorflow)
library(sp)
library(fields)
library(geoR)
use_condaenv("tf_gpu")
nychka_fun <- function(spdist, theta){
  
  d <- spdist/theta
  
  out <- matrix(NA, nrow = nrow(spdist), ncol = ncol(spdist))
  
  out[which(d > 1)] <- 0
  non_0 <- which(d<=1)
  out[non_0] <- (1-d[non_0])^6 * (35*d[non_0]^2 + 18*d[non_0] + 3)/3
  return(out)
}
nychka_fun <- function(spdist, theta){
  
  d <- spdist/theta
  
  out <- matrix(NA, nrow = nrow(spdist), ncol = ncol(spdist))
  
  out[which(d > 1)] <- 0
  non_0 <- which(d<=1)
  out[non_0] <- (1-d[non_0])^6 * (35*d[non_0]^2 + 18*d[non_0] + 3)/3
  return(out)
}

# Create basis function for DK
basis_1 <- expand.grid(seq(from = 0, to = 1, length.out = 10),seq(from = 0, to = 1, length.out = 10))  
basis_2 <- expand.grid(seq(from = 0, to = 1, length.out = 19),seq(from = 0, to = 1, length.out = 19))
basis_3 <- expand.grid(seq(from = 0, to = 1, length.out = 37),seq(from = 0, to = 1, length.out = 37))


sim_size = 30
sim_coords <- expand.grid(seq(0,1,length.out = sim_size),seq(0,1,length.out = sim_size))
sim_sbar <- (sim_coords[,1] + sim_coords[,2])/2
sim_y <- sin(30*(sim_sbar-0.9)^4) * cos(2*(sim_sbar-0.9)) + (sim_sbar-0.9)/2
p_obs <- 
  ggplot() +
  geom_raster(aes(x = sim_coords[,1], y = sim_coords[,2], fill = sim_y)) +
  scale_fill_viridis_c(name = "") + 
  labs(x = "Longitude",  y = "Latitude")

basis_dist_1 <- spDists(as.matrix(sim_coords), as.matrix(basis_1))
basis_dist_2 <- spDists(as.matrix(sim_coords), as.matrix(basis_2))
basis_dist_3 <- spDists(as.matrix(sim_coords), as.matrix(basis_3))

theta_1 <- 2.5* diff(seq(from = 0, to = 1, length.out = 10))[1]
theta_2 <- 2.5* diff(seq(from = 0, to = 1, length.out = 19))[1]
theta_3 <- 2.5* diff(seq(from = 0, to = 1, length.out = 37))[1]

basis_fun_1 <- nychka_fun(basis_dist_1, theta = theta_1)
basis_fun_2 <- nychka_fun(basis_dist_2, theta = theta_2)
basis_fun_3 <- nychka_fun(basis_dist_3, theta = theta_3)

pair_dist_2d <- spDists( as.matrix(sim_coords) )
set.seed(0)
train_all_index <- sample(1:10, sim_size^2, replace = TRUE)
krig_mean_all <- rep(NA, sim_size^2)
dkrig_mean_all <- rep(NA, sim_size^2)
ckrig_mean_all <- rep(NA, sim_size^2)
nn_mean_all <- rep(NA, sim_size^2)
mse_vec_krig <- rep(NA, 10)
mse_vec_nn <- rep(NA, 10)
mse_vec_dkrig <- rep(NA, 10)
mse_vec_ckrig <- rep(NA,10)


deep_learning <- function(model, x_tr, y_tr, x_te, y_te, epoch_stop)
{
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mse")
  )
  
  model_checkpoint <- callback_model_checkpoint(
    filepath = "C:/Users/10616/Desktop/temp/best_weights.h5",
    save_best_only = TRUE,
    monitor = "val_loss",
    mode = "min",
    verbose = 1
  )
  history <- model %>%
    fit(x = x_tr, y = y_tr, epochs = epoch_stop, batch_size = 64, callbacks = list(model_checkpoint), validation_data = list(x_te, y_te))
  model %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  return(model)
}


model_dnn <- keras_model_sequential()
model_dnn %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(ncol(sim_coords)), kernel_initializer = 'he_uniform') %>% 
  layer_dense(units = 100, activation = 'relu') %>% 
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'linear')

for (curr_index in 1:10) {
  train_index <- which(train_all_index != curr_index)
  train_coords <- sim_coords[train_index,]
  train_y <- sim_y[train_index]
  test_coords <- sim_coords[-train_index,]
  test_y <- sim_y[-train_index]
  # dnn_mean_all 
  
  x_tr <- array_reshape( as.matrix(train_coords), c(length(train_y), 2))
  x_te <- array_reshape( as.matrix(test_coords), c(length(test_y), 2)) 
  
  y_tr <- train_y
  y_te <- test_y
  # mse_epoch <- rep(NA, 200)
  # epoch_pred <- matrix(NA, nrow = 200, ncol = length(test_y))
  
  curr_model <- deep_learning(model_dnn, x_tr, y_tr, x_te, y_te, epoch_stop = 200)
  
  nn_mean_all[-train_index] <- predict(curr_model, x_te)
  mse_vec_nn[curr_index] <- evaluate(curr_model, x_te, y_te)[2]
}
