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

load(here::here("other_dataset/satellite/AllSatelliteTemps.RData"))
sat_dat <- na.omit(all.sat.temps[,c(1,2,4)])
colnames(sat_dat) <- c("long","lat", "y")
long <- sat_dat$long
lat <- sat_dat$lat
y <- sat_dat$y


coordinates(sat_dat) <- ~ long + lat
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


pred_dk <-  matrix(NA, nrow = length(y), ncol = num_sample)
pred_dk_g <-  matrix(NA, nrow = grid_res^2, ncol = num_sample)

grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]

# Basis Generating

gridbasis1 <- auto_basis(mainfold = plane(), data = sat_dat, nres = 1, type = "Gaussian", regular = 1)
gridbasis2 <- auto_basis(mainfold = plane(), data = sat_dat, nres = 2, type = "Gaussian", regular = 1)
gridbasis3 <- auto_basis(mainfold = plane(), data = sat_dat, nres = 3, type = "Gaussian", regular = 1)

show_basis(gridbasis3) + 
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude")

basis_1 <- matrix(NA, nrow = nrow(sat_dat), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1[,i] <- gridbasis1@fn[[i]](coordinates(sat_dat))
}

basis_1_g <- matrix(NA, nrow = nrow(cbind(g_long, g_lat)), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1_g[,i] <- gridbasis1@fn[[i]](coordinates(cbind(g_long, g_lat)))
}


basis_2 <- matrix(NA, nrow = nrow(sat_dat), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2[,i] <- gridbasis2@fn[[i]](coordinates(sat_dat))
}

basis_2_g <- matrix(NA, nrow = nrow(cbind(g_long, g_lat)), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2_g[,i] <- gridbasis2@fn[[i]](coordinates(cbind(g_long, g_lat)))
}



basis_3 <- matrix(NA, nrow = nrow(sat_dat), ncol = length(gridbasis3@fn))
for (i in 1:length(gridbasis3@fn)) {
  basis_3[,i] <- gridbasis3@fn[[i]](coordinates(sat_dat))
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
basis_arr_1 <- array(NA, dim = c(nrow(sat_dat), shape_row_1, shape_col_1))
basis_arr_1_g <- array(NA, dim = c(length(g_long), shape_row_1, shape_col_1))

for (i in 1:nrow(sat_dat)) {
  basis_arr_1[i,,] <- matrix(basis_use_1_2d[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = T)
}

for (i in 1:length(g_long)) {
  basis_arr_1_g[i,,] <- matrix(basis_use_1_2d_g[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = T)
}

# Second resolution
shape_row_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 2 ]))
shape_col_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 1 ]))
basis_arr_2 <- array(NA, dim = c(nrow(sat_dat), shape_row_2, shape_col_2))
basis_arr_2_g <- array(NA, dim = c(length(g_long), shape_row_2, shape_col_2))
for (i in 1:nrow(sat_dat)) {
  basis_arr_2[i,,] <- matrix(basis_use_2_2d[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = T)
}
for (i in 1:length(g_long)) {
  basis_arr_2_g[i,,] <- matrix(basis_use_2_2d_g[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = T)
}


# Third resolution
shape_row_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 2 ]))
shape_col_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 1 ]))
basis_arr_3 <- array(NA, dim = c(nrow(sat_dat), shape_row_3, shape_col_3))
basis_arr_3_g <- array(NA, dim = c(length(g_long), shape_row_3, shape_col_3))
for (i in 1:nrow(sat_dat)) {
  basis_arr_3[i,,] <- matrix(basis_use_3_2d[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = T)
}
for (i in 1:length(g_long)) {
  basis_arr_3_g[i,,] <- matrix(basis_use_3_2d_g[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = T)
}

pred_drop_layer <- layer_dropout(rate=pred_drop)


#CNN
set.seed(0)
sat_dat <- data.frame(long = long, lat = lat, y = y)  
fold_number <- sample(1:num_fold,nrow(sat_dat), replace = TRUE)
train_index_all <- sample(1:num_fold, nrow(sat_dat), replace = T)
for (curr_index in 1:num_fold) {
  
  tr_idx <- which(fold_number != curr_index)
  te_idx <- which(fold_number == curr_index)
  train_index <- sample(1:length(tr_idx), floor(0.9*length(tr_idx)))
  
  basis_tr_1  <- basis_1[tr_idx[train_index],]
  basis_te_1  <- basis_1[tr_idx[-train_index],]
  
  basis_tr_2  <- basis_2[tr_idx[train_index],]
  basis_te_2  <- basis_2[tr_idx[-train_index],]
  
  basis_tr_3  <- basis_3[tr_idx[train_index],]
  basis_te_3  <- basis_3[tr_idx[-train_index],]
  x_tr_tr <- cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)))[tr_idx[train_index],],basis_tr_1, basis_tr_2, basis_tr_3)
  x_tr_va <- cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)))[tr_idx[-train_index],],basis_te_1, basis_te_2, basis_te_3)
  
  y_tr_tr <- y[tr_idx[train_index]]
  y_tr_va <- y[tr_idx[-train_index]]
  
  
  dk_input_layer <- layer_input(shape = c(ncol(x_tr_tr))) 
  
  dk_output_layer <- dk_input_layer %>% 
    layer_dense(units = 100, activation = 'relu', kernel_initializer = "he_uniform") %>% 
    pred_drop_layer(training = T) %>%
    layer_batch_normalization() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    pred_drop_layer(training = T) %>%
    layer_batch_normalization()%>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')
  
  
  
  model_dk <- keras_model(dk_input_layer, dk_output_layer)
  
  # Compile the model
  
  model_dk %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mse")
  )
  
  model_checkpoint <- callback_model_checkpoint(
    filepath = "D:/77/research/temp/best_weights.h5",
    save_best_only = TRUE,
    monitor = "val_loss",
    mode = "min",
    verbose = 1
  )
  
  
  mod_train_dk <- model_dk %>%
    fit(x = x_tr_tr, y = y_tr_tr, epochs =1000, batch_size = 2000, 
        validation_data = list(x_tr_va, y_tr_va) , callbacks = model_checkpoint)
  
  
  model_dk %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  
  for (j in 1:num_sample) {
    print(j)
    pred_dk[te_idx,j] <- predict(model_dk,  cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)))[-tr_idx,], basis_1[-tr_idx,], basis_2[-tr_idx,], basis_3[-tr_idx,]))
    pred_dk_g[,j] <- predict(model_dk,cbind((g_long- min(long))/diff(range(long)), (g_lat - min(lat))/diff(range(lat)) ,basis_1_g, basis_2_g, basis_3_g))
    }
}

mean((apply(pred_dk, 1 , mean) - y)^2)
write.csv(as.data.frame(pred_dk),"D:/77/Research/temp/sat_pred/dk_pred_sat.csv",row.names = FALSE)
write.csv(as.data.frame(pred_dk_g),"D:/77/Research/temp/sat_pred/dk_pred_sat_g.csv",row.names = FALSE)
