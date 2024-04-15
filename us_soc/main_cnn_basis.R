rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(reticulate)
library(keras)
library(tensorflow)
library(sp)
library(fields)
library(FRK)
use_condaenv("tf_gpu")

us_soc <- read.csv(here::here("us_soc/soc_2.csv"))
long <- us_soc$long
lat <- us_soc$lat
y <- us_soc$soc

us_soc <- na.omit(as.data.frame(cbind(long, lat, y)))
long <- us_soc$long
lat <- us_soc$lat
y <- us_soc$y
coordinates(us_soc) <- ~ long + lat

grid_res <- 200
# Parameters setting
set.seed(0)
num_fold <- 5
num_sample <- 100
pred_drop <- 0.1

min_max_scale <- function(x)
{
  low <- range(x)[1]
  high <- range(x)[2]
  
  out <- (x - low)/(high - low)
  return(out)
  
}


pred_ck <-  matrix(NA, nrow = length(y), ncol = num_sample)
pred_ck_g <-  matrix(NA, nrow = grid_res^2, ncol = num_sample)

grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]

# Basis Generating

gridbasis1 <- auto_basis(mainfold = plane(), data = us_soc, nres = 1, type = "Gaussian", regular = 1)
gridbasis2 <- auto_basis(mainfold = plane(), data = us_soc, nres = 2, type = "Gaussian", regular = 1)
gridbasis3 <- auto_basis(mainfold = plane(), data = us_soc, nres = 3, type = "Gaussian", regular = 1)

show_basis(gridbasis3) + 
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude")

basis_1 <- matrix(NA, nrow = nrow(us_soc), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1[,i] <- gridbasis1@fn[[i]](coordinates(us_soc))
}

basis_1_g <- matrix(NA, nrow = nrow(cbind(g_long, g_lat)), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1_g[,i] <- gridbasis1@fn[[i]](coordinates(cbind(g_long, g_lat)))
}


basis_2 <- matrix(NA, nrow = nrow(us_soc), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2[,i] <- gridbasis2@fn[[i]](coordinates(us_soc))
}

basis_2_g <- matrix(NA, nrow = nrow(cbind(g_long, g_lat)), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2_g[,i] <- gridbasis2@fn[[i]](coordinates(cbind(g_long, g_lat)))
}



basis_3 <- matrix(NA, nrow = nrow(us_soc), ncol = length(gridbasis3@fn))
for (i in 1:length(gridbasis3@fn)) {
  basis_3[,i] <- gridbasis3@fn[[i]](coordinates(us_soc))
}

basis_3_g <- matrix(NA, nrow = nrow(cbind(g_long, g_lat)), ncol = length(gridbasis3@fn))
for (i in 1:length(gridbasis3@fn)) {
  basis_3_g[,i] <- gridbasis3@fn[[i]](coordinates(cbind(g_long, g_lat)))
}


# Redefine three layers of basis images
basis_use_1_2d <- basis_1
basis_use_2_2d <- basis_3[,(ncol(basis_1)+1):ncol(basis_2)]
basis_use_3_2d <- basis_3[,(ncol(basis_2)+1):ncol(basis_3)]

basis_use_1_2d_g <- basis_1_g
basis_use_2_2d_g <- basis_3_g[,(ncol(basis_1_g)+1):ncol(basis_2_g)]
basis_use_3_2d_g <- basis_3_g[,(ncol(basis_2_g)+1):ncol(basis_3_g)]


# First resolution
shape_row_1 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 1) , 2 ]))
shape_col_1 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 1) , 1 ]))
basis_arr_1 <- array(NA, dim = c(nrow(us_soc), shape_row_1, shape_col_1))
basis_arr_1_g <- array(NA, dim = c(length(g_long), shape_row_1, shape_col_1))

for (i in 1:nrow(us_soc)) {
  basis_arr_1[i,,] <- matrix(basis_use_1_2d[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = T)
}

for (i in 1:length(g_long)) {
  basis_arr_1_g[i,,] <- matrix(basis_use_1_2d_g[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = T)
}

# Second resolution
shape_row_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 2 ]))
shape_col_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 1 ]))
basis_arr_2 <- array(NA, dim = c(nrow(us_soc), shape_row_2, shape_col_2))
basis_arr_2_g <- array(NA, dim = c(length(g_long), shape_row_2, shape_col_2))
for (i in 1:nrow(us_soc)) {
  basis_arr_2[i,,] <- matrix(basis_use_2_2d[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = T)
}
for (i in 1:length(g_long)) {
  basis_arr_2_g[i,,] <- matrix(basis_use_2_2d_g[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = T)
}


# Third resolution
shape_row_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 2 ]))
shape_col_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 1 ]))
basis_arr_3 <- array(NA, dim = c(nrow(us_soc), shape_row_3, shape_col_3))
basis_arr_3_g <- array(NA, dim = c(length(g_long), shape_row_3, shape_col_3))
for (i in 1:nrow(us_soc)) {
  basis_arr_3[i,,] <- matrix(basis_use_3_2d[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = T)
}
for (i in 1:length(g_long)) {
  basis_arr_3_g[i,,] <- matrix(basis_use_3_2d_g[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = T)
}

pred_drop_layer <- layer_dropout(rate=pred_drop)


#CNN
set.seed(0)
us_soc <- data.frame(long = long, lat = lat, y = y)  
fold_number <- sample(1:num_fold,nrow(us_soc), replace = TRUE)
train_index_all <- sample(1:num_fold, nrow(us_soc), replace = T)

for (curr_index in 1:num_fold) {
  
  tr_idx <- which(fold_number != curr_index)
  te_idx <- which(fold_number == curr_index)
  train_index <- sample(1:length(tr_idx), floor(0.9*length(tr_idx)))
  
  basis_tr_1 <- array_reshape(basis_arr_1[tr_idx[train_index],,], c(length(tr_idx[train_index]), shape_row_1, shape_col_1, 1))
  basis_tr_2 <- array_reshape(basis_arr_2[tr_idx[train_index],,], c(length(tr_idx[train_index]), shape_row_2, shape_col_2, 1))
  basis_tr_3 <- array_reshape(basis_arr_3[tr_idx[train_index],,], c(length(tr_idx[train_index]), shape_row_3, shape_col_3, 1))
  
  basis_te_1 <- array_reshape(basis_arr_1[tr_idx[-train_index],,], c(length(tr_idx[-train_index]), shape_row_1, shape_col_1, 1))
  basis_te_2 <- array_reshape(basis_arr_2[tr_idx[-train_index],,], c(length(tr_idx[-train_index]), shape_row_2, shape_col_2, 1))
  basis_te_3 <- array_reshape(basis_arr_3[tr_idx[-train_index],,], c(length(tr_idx[-train_index]), shape_row_3, shape_col_3, 1))
  
  cov_tr_tr <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat)))[tr_idx[train_index],]
  cov_tr_va <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat)))[tr_idx[-train_index],]
  
  basis_TE_1 <- array_reshape(basis_arr_1[-tr_idx,,], c(length(y) - length(tr_idx), shape_row_1, shape_col_1, 1))
  basis_TE_2 <- array_reshape(basis_arr_2[-tr_idx,,], c(length(y) - length(tr_idx), shape_row_2, shape_col_2, 1))
  basis_TE_3 <- array_reshape(basis_arr_3[-tr_idx,,], c(length(y) - length(tr_idx), shape_row_3, shape_col_3, 1))
  cov_TE <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat)))[-tr_idx,]
  
  # We need three convolutional input model and adding covariates.
  input_basis_1 <- layer_input(shape = c(shape_row_1, shape_col_1, 1))
  input_basis_2 <- layer_input(shape = c(shape_row_2, shape_col_2, 1))
  input_basis_3 <- layer_input(shape = c(shape_row_3, shape_col_3, 1))
  input_cov <- layer_input(shape = ncol(cov_tr_tr))
  
  resolution_1_conv <- input_basis_1 %>%
    layer_conv_2d(filters = 128, kernel_size = c(2,2), activation = 'relu') %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu')
  
  resolution_2_conv <- input_basis_2 %>%
    layer_conv_2d(filters = 128, kernel_size = c(2,2), activation = 'relu') %>%
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu')
  
  resolution_3_conv <- input_basis_3 %>%
    layer_conv_2d(filters = 128, kernel_size = c(2,2), activation = 'relu') %>%
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu')
  
  cov_model <- input_cov %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) %>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_batch_normalization() %>%
    pred_drop_layer(training = T) 
  
  
  all_model <- layer_concatenate(list(resolution_1_conv, resolution_2_conv, resolution_3_conv, cov_model))
  
  output_layer <- all_model %>%  layer_dense(units = 1, activation = 'linear')
  
  model_ck <- keras_model(inputs = list(input_basis_1, input_basis_2, input_basis_3, input_cov), outputs = output_layer)
  
  model_ck <- 
    model_ck %>% compile(
      optimizer = "adam",
      loss = 'mse',
      metrics = c('mse')
    )
  
  model_checkpoint <- callback_model_checkpoint(
    filepath = "D:/77/research/temp/best_weights.h5",
    save_best_only = TRUE,
    monitor = "val_loss",
    mode = "min",
    verbose = 1
  )
  
  mod_train_ck <- model_ck %>% fit(
    x = list(basis_tr_1, basis_tr_2, basis_tr_3, cov_tr_tr),
    y = y[tr_idx[train_index]],
    epochs=1000,
    batch_size=500,
    validation_data=list(list(basis_te_1,basis_te_2,basis_te_3,cov_tr_va), y[tr_idx[-train_index]]),
    callbacks = model_checkpoint, shuffle = TRUE
  )
  
  model_ck %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  for (j in 1:num_sample) {
    print(j)
    pred_ck[te_idx,j] <- predict(model_ck, list(basis_TE_1,basis_TE_2,basis_TE_3,cov_TE))
    pred_ck_g[,j] <- predict(model_ck, list(basis_arr_1_g, basis_arr_2_g, basis_arr_3_g,
                      cbind((g_long- min(long))/diff(range(long)), (g_lat - min(lat))/diff(range(lat)))))
  }
  
}

mean((apply(pred_ck, 1 , mean) - y)^2)
write.csv(as.data.frame(pred_ck),"D:/77/Research/temp/soc_pred/ck_pred_soc.csv",row.names = FALSE)
write.csv(as.data.frame(pred_ck_g),"D:/77/Research/temp/soc_pred/ck_pred_soc_g.csv",row.names = FALSE)
