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


min_max_scale <- function(x)
{
  low <- range(x)[1]
  high <- range(x)[2]
  
  out <- (x - low)/(high - low)
  return(out)
  
}

sim_size = 300

egg_fun <- function(x,y){
  out <- -(500*y + 47)*sin(sqrt(abs(500*x/2 + 500*y + 47))) - 500*x*sin(sqrt(abs(500*x-(500*y + 47))))
}

long_grid = lat_grid <- seq(from = -1, to = 1, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]
eh_dat <- data.frame(long = long, lat = lat, y = y)  

coordinates(eh_dat) <- ~ long + lat

# Parameters setting
set.seed(0)
num_sample <- 100
pred_drop <- 0.1
num_fold <- 5

# pred_dnn <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_dk <- matrix(NA,nrow = length(y), ncol = num_sample)
# pred_ck <- matrix(NA,nrow = length(y), ncol = num_sample)
# pred_inla <- matrix(NA,nrow = length(y), ncol = num_sample)
# Basis Generating

gridbasis1 <- auto_basis(mainfold = plane(), data = eh_dat, nres = 1, type = "Gaussian", regular = 1)
gridbasis2 <- auto_basis(mainfold = plane(), data = eh_dat, nres = 2, type = "Gaussian", regular = 1)
gridbasis3 <- auto_basis(mainfold = plane(), data = eh_dat, nres = 3, type = "Gaussian", regular = 1)

show_basis(gridbasis3) + 
  coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude")

basis_1 <- matrix(NA, nrow = nrow(eh_dat), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1[,i] <- gridbasis1@fn[[i]](coordinates(eh_dat))
}

basis_2 <- matrix(NA, nrow = nrow(eh_dat), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2[,i] <- gridbasis2@fn[[i]](coordinates(eh_dat))
}

basis_3 <- matrix(NA, nrow = nrow(eh_dat), ncol = length(gridbasis3@fn))
for (i in 1:length(gridbasis3@fn)) {
  basis_3[,i] <- gridbasis3@fn[[i]](coordinates(eh_dat))
}


# Redefine three layers of basis images
basis_use_1_2d <- basis_1
basis_use_2_2d <- basis_3[,(ncol(basis_1)+1):ncol(basis_2)]
basis_use_3_2d <- basis_3[,(ncol(basis_2)+1):ncol(basis_3)]

# First resolution
shape_row_1 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 1) , 2 ]))
shape_col_1 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 1) , 1 ]))
basis_arr_1 <- array(NA, dim = c(nrow(eh_dat), shape_row_1, shape_col_1))
for (i in 1:nrow(eh_dat)) {
  basis_arr_1[i,,] <- matrix(basis_use_1_2d[i,], nrow = shape_row_1, ncol = shape_col_1, byrow = T)
}

# Second resolution
shape_row_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 2 ]))
shape_col_2 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 2) , 1 ]))
basis_arr_2 <- array(NA, dim = c(nrow(eh_dat), shape_row_2, shape_col_2))
for (i in 1:nrow(eh_dat)) {
  basis_arr_2[i,,] <- matrix(basis_use_2_2d[i,], nrow = shape_row_2, ncol = shape_col_2, byrow = T)
}

# Third resolution
shape_row_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 2 ]))
shape_col_3 <- length(table(gridbasis3@df[which(gridbasis3@df$res == 3) , 1 ]))
basis_arr_3 <- array(NA, dim = c(nrow(eh_dat), shape_row_3, shape_col_3))

for (i in 1:nrow(eh_dat)) {
  basis_arr_3[i,,] <- matrix(basis_use_3_2d[i,], nrow = shape_row_3, ncol = shape_col_3, byrow = T)
}


pred_drop_layer <- layer_dropout(rate=pred_drop)


#DNN

set.seed(0)

fold_number <- sample(1:num_fold,nrow(eh_dat), replace = TRUE)

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
  
  early_stopping <- callback_early_stopping(
    monitor = "val_loss",
    mode = "min",
    verbose = 1,
    patience = 200,
    restore_best_weights = TRUE
  )
  
  
  mod_train_dk <- model_dk %>%
    fit(x = x_tr_tr, y = y_tr_tr, epochs =1000, batch_size = 1000, 
        validation_data = list(x_tr_va, y_tr_va) , callbacks = model_checkpoint,
        verbose = 1)
  
  
  # model_dk %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  
  for (j in 1:num_sample) {
    print(j)
    pred_dk[te_idx, j] <- predict(model_dk,  cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)))[-tr_idx,], basis_1[-tr_idx,], basis_2[-tr_idx,], basis_3[-tr_idx,]))
  }
}

mean((apply(pred_dk, 1 , mean) - y)^2)
write.csv(pred_dk,"D:/77/Research/temp/eh_pred/dk_pred_eh.csv", row.names = FALSE)
