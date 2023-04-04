# For now we only visualize the spatial random effect, no fixed effect included. 


fix_tr <- fix_eff[train_index,]
basis_tr <- basis_arr[train_index,,]
soc_tr <- us_soc$soc[train_index]
state_tr <- us_soc$State[train_index]


fix_te <- fix_eff[-train_index,]
basis_te <- basis_arr[-train_index,,]
soc_te <- us_soc$soc[-train_index]
state_te <- us_soc$State[-train_index]

# Define a few parameters to be used in the CNN model
batch_size <- 128
epochs <- 50

# Input image dimensions
img_rows <- shape_row
img_cols <- shape_col



x_tr <- array_reshape(basis_tr, c(nrow(basis_tr), img_rows, img_cols, 1))
x_te <- array_reshape(basis_te, c(nrow(basis_te), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)



######################### Need Verify! ######################################################
#Example of the picture

index_image = 100 ## change this index to see different image. For now, we see the 1000th picture
input_matrix <- matrix( as.vector(basis_tr[index_image,,]), nrow = shape_row, ncol = shape_col)


exp_grid <- expand.grid(x = 1:21, y = 1:9)
ggplot() +
  geom_contour_filled(aes(x = exp_grid$x, y = exp_grid$y, z = as.vector(input_matrix))) 



######################### Verified! ######################################################




cnn_model<- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = input_shape) %>% 
  # layer_conv_2d(filters = 64, kernel_size = c(2,2), activation = 'relu') %>% 
  # layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.3) %>% 
  # layer_dense(units = 128, activation = 'relu') %>% 
  # layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 1, activation = 'linear')


cnn_model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae")
)

summary(cnn_model)


# Train the model

mod_train <- cnn_model %>%
  fit(x = x_tr, y = soc_tr, epochs = 50, batch_size = 128, 
      validation_split = 0.2)

# Test the model

loss_conv <- cnn_model %>%
  evaluate(x_te, soc_te)
loss_conv
