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

pred_dnn <- matrix(NA,nrow = length(y), ncol = num_sample)
# pred_dk <- matrix(NA,nrow = length(y), ncol = num_sample)
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

# train_index_all <- sample(1:num_fold, nrow(eh_tr), replace = T)

for (curr_index in 1:num_fold) {
  
  tr_idx <- which(fold_number != curr_index)
  te_idx <- which(fold_number == curr_index)
  train_index <- sample(1:length(tr_idx), floor(0.9*length(tr_idx)))
  
  
  x_tr_tr <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[tr_idx[train_index],]
  x_tr_va <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[tr_idx[-train_index],]
  
  y_tr_tr <- y[tr_idx[train_index]]
  y_tr_va <- y[tr_idx[-train_index]]
  
  
  dnn_input_layer <- layer_input(shape = c(ncol(x_tr_tr))) 
  
  dnn_output_layer <- dnn_input_layer %>% 
    layer_dense(units = 100, activation = 'relu',  kernel_initializer = "he_uniform") %>% 
    pred_drop_layer(training = T) %>%
    layer_batch_normalization()%>%
    layer_dense(units = 100, activation = 'relu') %>%
    pred_drop_layer(training = T) %>%
    layer_batch_normalization()%>%
    layer_dense(units = 100, activation = 'relu') %>%
    pred_drop_layer(training = T) %>%
    layer_batch_normalization()%>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  
  
  model_dnn <- keras_model(dnn_input_layer, dnn_output_layer)
  # Compile the model
  
  model_dnn %>% compile(
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
  
  mod_train_dnn <- model_dnn %>%
    fit(x = x_tr_tr, y = y_tr_tr, epochs = 1500, batch_size = 1000, 
        validation_data = list(x_tr_va, y_tr_va) , callbacks = list(model_checkpoint))
  
  model_dnn %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  
  for (j in 1:num_sample) {
    print(j)
    pred_dnn[te_idx,  ] <- predict(model_dnn, 
                                         as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[-tr_idx,]
    )
  }
  
}
# Check prediction MSE
mean((apply(pred_dnn, 1 , mean) - y)^2)

saveRDS(pred_dnn,"D:/77/Research/temp/eh_pred/dnn_pred_eh.rds")