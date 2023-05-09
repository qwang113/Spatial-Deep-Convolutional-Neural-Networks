curr_index <- 1
  train_index <- which(train_index_all != curr_index)

  
  x_tr <- as.matrix(basis_fun_4[train_index,])
  
  
  x_te <- as.matrix(basis_fun_4[-train_index,])
  # 
  # x_tr <- array_reshape (basis_tr, c(nrow(basis_tr),  shape_row*shape_col))  # So we want to reshape each observation from a picture to a vector
  # x_te <- array_reshape(basis_te, c(nrow(basis_te),  shape_row*shape_col))  # Same as prervious step
  # basis_tr  <- basis_3[train_index,]
  # basis_te  <- basis_3[-train_index,]
  # x_tr <- scale(as.matrix(cbind(cov_tr, basis_tr)))
  # x_te <- scale(as.matrix(cbind(cov_te, basis_te)))
  
  pm_tr <- pm[train_index]
  pm_te <- pm[-train_index]
  
  # Compile the model
  drop = 0.1
  model_dk <- keras_model_sequential() 
  
  model_dk %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = c( ncol(as.matrix(basis_fun_4[train_index,])) ), kernel_initializer = "he_uniform") %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    #layer_dropout(drop) %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    #layer_dropout(drop) %>%
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
  history <- model_dk %>%
    fit(x = x_tr, y = pm_tr, epochs = 200, batch_size = 32, callbacks = list(model_checkpoint), validation_data = list(x_te, pm_te))
  model_dk %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  
  loss_dk[curr_index] <- evaluate(model_dk, x_te, pm_te)[2]
  pred_dk[-train_index] <- predict(model_dk, x_te)

  
# -------------------------------  c k --------------------------------------
# Fourth resolution
  shape_col_4 <- shape_row_4 <- 73
  basis_arr_4 <- array(NA, dim = c(length(pm), shape_row_4, shape_col_4))
  for (i in 1:length(pm)) {
    basis_arr_4[i,,] <- matrix(basis_fun_4[i,], nrow = shape_row_4, ncol = shape_col_4)
  }
  
  
  basis_tr_4 <- array_reshape(basis_arr_4[train_index,,], c(length(train_index), shape_row_4, shape_col_4, 1))
  
  basis_te_4 <- array_reshape(basis_arr_4[-train_index,,], c(length(pm) - length(train_index), shape_row_4, shape_col_4, 1))
  
  model_ck  <- keras_model_sequential() %>%
    layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu', input_shape = c(shape_row_4, shape_col_4, 1)) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 100, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')
  
  
  
  
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
    x = basis_tr_4,
    y = pm[train_index],
    epochs=1000,
    batch_size=16,
    validation_data=list(basis_te_4, pm[-train_index]),
    callbacks = model_checkpoint
  )
  
  model_ck %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  loss_ck[curr_index] <- evaluate(model_ck, basis_te_4, pm[-train_index])[2]

  
  
  
  
  
  
  
  
  