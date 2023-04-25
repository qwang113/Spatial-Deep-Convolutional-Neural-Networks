max_temp <- 5
train_temp <- sample(1:max_temp, 900, replace = TRUE)
epoc_temp <- 100



for (curr_index in 1:max_temp) {
  train_index <- which(train_all_index != curr_index)
  train_coords <- sim_coords[train_index,]
  train_y <- sim_y[train_index]
  test_coords <- sim_coords[-train_index,]
  test_y <- sim_y[-train_index]
  
  x_tr <- cbind(train_coords, basis_fun_1[train_index,],
                basis_fun_2[train_index,],basis_fun_3[train_index,])
  x_te <- cbind(test_coords, basis_fun_1[-train_index,],
                basis_fun_2[-train_index,],basis_fun_3[-train_index,])
  # x_tr <- train_coords
  # x_te <- test_coords
  x_tr <- array_reshape( as.matrix(x_tr), c(length(train_y), ncol(x_tr)))
  x_te <- array_reshape( as.matrix(x_te), c(length(test_y), ncol(x_te)))
  
  model_dk <- keras_model_sequential()
  model_dk %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = c(ncol(x_tr))) %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')
  model_dk %>% compile(
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
  mod_train_dk <- model_dk %>%
    fit(x = x_tr, y = train_y, epochs = epoc_temp, batch_size = 64, 
        callbacks = list(model_checkpoint), validation_data = list(x_te, test_y))
  model_dk %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  dkrig_mean_all[-train_index] <- predict(model_dk, x_te)
  mse_vec_dkrig[curr_index] <- evaluate(model_dk, x_te, test_y)[2]
  
}


p_dk <- 
  ggplot() +
  geom_raster(aes(x = sim_coords[,1], y = sim_coords[,2], fill = dkrig_mean_all)) +
  scale_fill_viridis_c(name = "") + 
  labs(x = "Longitude",  y = "Latitude")

# --------------------------------------------------------------------- DNN

for (curr_index in 1:max_temp) {
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

  model_dnn <- keras_model_sequential()
  model_dnn %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = c(ncol(x_tr))) %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 100, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  
  model_dnn %>% compile(
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
  dnn_history <- model_dnn %>%
    fit(x = x_tr, y = y_tr, epochs = epoc_temp, batch_size = 64, 
        callbacks = list(model_checkpoint), validation_data = list(x_te, test_y))
  model_dnn %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  
  nn_mean_all[-train_index] <- predict(model_dnn, x_te)
  mse_vec_nn[curr_index] <- evaluate(model_dnn, x_te, y_te)[2]
  
}
p_dnn <- 
  ggplot() +
  geom_raster(aes(x = sim_coords[,1], y = sim_coords[,2], fill = nn_mean_all)) +
  scale_fill_viridis_c(name = "") + 
  labs(x = "Longitude",  y = "Latitude")
