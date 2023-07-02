
# Prediction with an empty area DNN -------------------------------------------------------------------


for (curr_index in 1:num_fold) {
  train_index_temp <- which(train_index_all != curr_index)
  train_index <- train_index_temp[!(train_index_temp %in% test_area_index)]
  
  x_tr <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[train_index,]
  x_te <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat))  )[-train_index,]
  
  y_tr <- y[train_index]
  y_te <- y[-train_index]
  
  dnn_input_layer <- layer_input(shape = c(ncol(x_tr))) 
  
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
    fit(x = x_tr, y = y_tr, epochs = 500, batch_size = 1000, 
        validation_data = list(x_te, y_te) , callbacks = list(model_checkpoint))
  
  model_dnn %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  
  for (j in 1:num_sample) {
    print(j)
    pred_empty_area_dnn[-train_index,j] <- predict(model_dnn, x_te)
  }
  
  
}









# Prediction with an empty area DK --------------------------------------------------------------


for (curr_index in 1:num_fold) {
  
  train_index_temp <- which(train_index_all != curr_index)
  train_index <- train_index_temp[!(train_index_temp %in% test_area_index)]
  
  basis_tr_1  <- basis_1[train_index,]
  basis_te_1  <- basis_1[-train_index,]
  
  basis_tr_2  <- basis_2[train_index,]
  basis_te_2  <- basis_2[-train_index,]
  
  basis_tr_3  <- basis_3[train_index,]
  basis_te_3  <- basis_3[-train_index,]
  x_tr <- cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)) * 10)[train_index,],basis_tr_1, basis_tr_2, basis_tr_3)
  x_te <- cbind(as.matrix(cbind(min_max_scale(long),min_max_scale(lat)) * 10)[-train_index,],basis_te_1, basis_te_2, basis_te_3)
  
  y_tr <- y[train_index]
  y_te <- y[-train_index]
  
  
  dk_input_layer <- layer_input(shape = c(ncol(x_tr))) 
  
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
    fit(x = x_tr, y = y_tr, epochs =200, batch_size = 1000, 
        validation_data = list(x_te, y_te) , callbacks = model_checkpoint)
  
  
  model_dk %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  
  for (j in 1:num_sample) {
    print(j)
    pred_empty_area_dk[-train_index,j] <- predict(model_dk, x_te)
  }
  
}






# Prediction with empty area CK --------------------------------------------------------------

for (curr_index in 1:num_fold) {
  
  train_index_temp <- which(train_index_all != curr_index)
  train_index <- train_index_temp[!(train_index_temp %in% test_area_index)]
  
  basis_tr_1 <- array_reshape(basis_arr_1[train_index,,], c(length(train_index), shape_row_1, shape_col_1, 1))
  basis_tr_2 <- array_reshape(basis_arr_2[train_index,,], c(length(train_index), shape_row_2, shape_col_2, 1))
  basis_tr_3 <- array_reshape(basis_arr_3[train_index,,], c(length(train_index), shape_row_3, shape_col_3, 1))
  
  basis_te_1 <- array_reshape(basis_arr_1[-train_index,,], c(length(y) - length(train_index), shape_row_1, shape_col_1, 1))
  basis_te_2 <- array_reshape(basis_arr_2[-train_index,,], c(length(y) - length(train_index), shape_row_2, shape_col_2, 1))
  basis_te_3 <- array_reshape(basis_arr_3[-train_index,,], c(length(y) - length(train_index), shape_row_3, shape_col_3, 1))
  
  cov_tr <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat)) * 10)[train_index,]
  cov_te <- as.matrix( cbind(min_max_scale(long),min_max_scale(lat)) * 10)[-train_index,]
  
  # We need three convolutional input model and adding covariates.
  input_basis_1 <- layer_input(shape = c(shape_row_1, shape_col_1, 1))
  input_basis_2 <- layer_input(shape = c(shape_row_2, shape_col_2, 1))
  input_basis_3 <- layer_input(shape = c(shape_row_3, shape_col_3, 1))
  input_cov <- layer_input(shape = ncol(cov_tr))
  
  
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
    layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>%
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
    x = list(basis_tr_1, basis_tr_2, basis_tr_3, cov_tr),
    y = y[train_index],
    epochs=200,
    batch_size=1000,
    validation_data=list(list(basis_te_1,basis_te_2,basis_te_3,cov_te), y[-train_index]),
    callbacks = model_checkpoint, shuffle = TRUE
  )
  
  model_ck %>% load_model_weights_hdf5("D:/77/research/temp/best_weights.h5")
  
  for (j in 1:num_sample) {
    print(j)
    pred_empty_area_ck[-train_index,j] <- predict(model_ck, list(basis_te_1,basis_te_2,basis_te_3,cov_te))
  }
  
}




# Prediction with empty area INLA --------------------------------------------------------------

for (curr_index in 1:num_fold) {
  train_index <- which(curr_index != train_index_all[-test_area_index])
  long <- min_max_scale(long) * 10
  lat <- min_max_scale(lat) * 10
  long_tr <- long[train_index]
  lat_tr <- lat[train_index]
  y_tr <- y[train_index]
  coords <- cbind(long_tr, lat_tr)
  
  non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
  mesh4 <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(0.5,1), 
                        offset = c(0.5, 1),
                        cutoff = 0.3)
  
  
  
  A<-inla.spde.make.A(mesh=mesh4,loc=as.matrix(coords))
  
  
  spde <- inla.spde2.matern(mesh4, alpha=0.5)
  iset <- inla.spde.make.index(name = "spatial.field", spde$n.spde)
  
  stk <- inla.stack(data=list(y=y_tr), #the response
                    
                    A=list(A,1),  #the A matrix; the 1 is included to make the list(covariates)
                    
                    effects=list(c(list(Intercept=1), #the Intercept
                                   iset),  #the spatial index
                                 #the covariates
                                 list(long = long_tr,
                                      lat = lat_tr)
                    ), 
                    
                    #this is a quick name so you can call upon easily
                    tag='dat')
  
  
  formula0<- y ~ -1 + long + lat + f(spatial.field, model=spde) 
  
  model0<-inla(formula0, #the formula
               data=inla.stack.data(stk,spde=spde),  #the data stack
               family= 'gaussian',   #which family the data comes from
               control.predictor=list(A=inla.stack.A(stk),compute=TRUE),  #compute gives you the marginals of the linear predictor
               control.compute = list(dic = TRUE, waic = TRUE, config = TRUE, return.marginals.predictor = TRUE), #model diagnostics and config = TRUE gives you the GMRF
               verbose = TRUE) #can include verbose=TRUE to see the log of the model runs
  
  
  
  long_te <- long[-train_index]
  lat_te <- lat[-train_index]
  y_te <- y[-train_index]
  
  
  test_coords <- cbind(long_te, lat_te)
  
  
  # Prediction
  Aprediction <- inla.spde.make.A(mesh = mesh4, loc = test_coords);
  
  stk.pred <- inla.stack(data=list(y=NA), 
                         A=list(Aprediction,1), 
                         effects=list(c(list(Intercept=1)
                                        ,iset),
                                      list(
                                        long = long_te,
                                        lat = lat_te
                                      )
                         ), 
                         tag='pred')
  
  #join the prediction stack with the one for the full data
  stk.full <- inla.stack(stk, stk.pred)
  
  p.res.pred<-inla(formula0, data=inla.stack.data(stk.full,spde=spde),
                   family= 'gaussian', quantiles = NULL,
                   control.predictor=list(link = 1, A=inla.stack.A(stk.full),compute=TRUE),  #compute gives you the marginals of the linear predictor
                   control.compute = list(config = TRUE), #model diagnostics and config = TRUE gives you the GMRF
                   control.inla(strategy = 'simplified.laplace', huge = TRUE),  #this is to make it run faster
                   verbose = FALSE) 
  
  index.pred<-inla.stack.index(stk.full, "pred")$data
  
  # <- inla.posterior.sample(n = num_sample, p.res.pred)
  temp_pos_sample <- inla.posterior.sample(num_sample,p.res.pred)
  pred_empty_area_inla[-train_index,] <- matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample)
  
}

