for (curr_index in 1:num_fold) {
  train_index <- which(train_index_all != curr_index)

  num_0 <- which(colSums(basis_fun_3[train_index,])==0)
  
  x_tr <- as.matrix(cbind(basis_fun_3[train_index,]))[,-num_0]
  
  x_te <- as.matrix(cbind(basis_fun_3[-train_index,]))[,-num_0]

  pm_tr <- acc_num[train_index]
  pm_te <- acc_num[-train_index]
  
  # Compile the model
  drop = 0.1
  model_dk <- keras_model_sequential() 
  
  model_dk %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = c(ncol(basis_fun_3[train_index,]) - length(num_0) ), kernel_initializer = "he_uniform") %>% 
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
    fit(x = x_tr, y = pm_tr, epochs = 1000, batch_size = 512, callbacks = list(model_checkpoint), validation_data = list(x_te, pm_te))
  model_dk %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  
  loss_dk[curr_index] <- evaluate(model_dk, x_te, pm_te)[2]
  pred_dk[-train_index] <- predict(model_dk, x_te)
}




for (curr_index in 1:num_fold) {
  
  train_index <- which(train_index_all != curr_index)
  
  basis_tr_3 <- array_reshape(basis_arr_3[train_index,,], c(length(train_index), shape_row_3, shape_col_3, 1))
 
  basis_te_3 <- array_reshape(basis_arr_3[-train_index,,], c(length(acc_num) - length(train_index), shape_row_3, shape_col_3, 1))
  
  drop = 0.1
  
  model_ck <- keras_model_sequential()
  
  model_ck %>% 
  layer_conv_2d(filters = 100, kernel_size = c(2,2), activation = 'relu',input_shape = c(shape_row_3, shape_col_3, 1)) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = 'relu') %>% 
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
    x = basis_tr_3,
    y = acc_num[train_index],
    epochs=1000,
    batch_size=512,
    validation_data= list(basis_te_3, acc_num[-train_index]),
    callbacks = model_checkpoint
  )
  
  model_ck %>% load_model_weights_hdf5("C:/Users/10616/Desktop/temp/best_weights.h5")
  
  loss_ck[curr_index] <- evaluate(model_ck, list(basis_te_3, acc_num[-train_index]))[2]
  pred_ck[-train_index] <- predict(model_ck, basis_te_3)
  
}


