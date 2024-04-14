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


pred_dnn <-  matrix(NA, nrow = length(y), ncol = num_sample)
pred_dnn_g <-  matrix(NA, nrow = grid_res^2, ncol = num_sample)

grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]

pred_drop_layer <- layer_dropout(rate=pred_drop)

#DNN
set.seed(0)
us_soc <- data.frame(long = long, lat = lat, y = y)  
fold_number <- sample(1:num_fold,nrow(us_soc), replace = TRUE)


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
    pred_dnn[te_idx,j] <- predict(model_dnn, 
                                         as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[-tr_idx,])
    pred_dnn_g[,j] <- predict(model_dnn,cbind((g_long- min(long))/diff(range(long)), (g_lat - min(lat))/diff(range(lat)) ) )
  }
  
}
# Check prediction MSE
mean((apply(pred_dnn, 1 , mean) - y)^2)
write.csv(as.data.frame(pred_dnn),"D:/77/Research/temp/soc_pred/dnn_pred_soc.csv",row.names = FALSE)
write.csv(as.data.frame(pred_dnn_g),"D:/77/Research/temp/soc_pred/dnn_pred_soc_g.csv",row.names = FALSE)

