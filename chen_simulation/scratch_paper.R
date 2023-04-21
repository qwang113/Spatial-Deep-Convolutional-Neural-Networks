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
  
  
  model_dnn <- keras_model_sequential()
  model_dnn %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = c(ncol(x_tr)), kernel_initializer = 'he_uniform') %>% 
    layer_batch_normalization() %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_batch_normalization() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_batch_normalization() %>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_batch_normalization() %>%
    layer_dense(units = 1, activation = 'linear')
  
  model_dnn %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mse")
  )
  
  mod_train_dnn <- model_dnn %>%
    fit(x = x_tr, y = y_tr, epochs = 200, batch_size = 64)
  
  nn_mean_all[-train_index] <- predict(model_dnn, x_te)
  mse_vec_nn[curr_index] <- evaluate(model_dnn, x_te, y_te)[2]

}


p_dnn <- 
  ggplot() +
  geom_raster(aes(x = sim_coords[,1], y = sim_coords[,2], fill = nn_mean_all)) +
  scale_fill_viridis_c(name = "") + 
  labs(x = "Longitude",  y = "Latitude")
