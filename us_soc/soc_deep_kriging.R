library(reticulate)
library(tensorflow)
library(keras)
set.seed(0)
train_index <- sample(1:nrow(us_soc), 4000, replace = FALSE)

fix_tr <- fix_eff[train_index,]
basis_tr <- basis_arr[train_index,,]
soc_tr <- us_soc$soc[train_index]


fix_te <- fix_eff[-train_index,]
basis_te <- basis_arr[-train_index,,]
soc_te <- us_soc$soc[-train_index]

basis_tr <- array_reshape(basis_tr, c(nrow(basis_tr), shape_row*shape_col))  # So we want to reshape each observation from a picture to a vector
basis_te <- array_reshape(basis_te, c(nrow(basis_te), shape_row*shape_col))  # Same as prervious step


x_tr <- cbind(fix_tr, basis_tr)
x_te <- cbind(fix_te, basis_te)

# Build up the model

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(ncol(basis_tr))) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')

# Compile the model

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mae")
)


# Train the model

mod_train <- model %>%
  fit(x = basis_tr, y = soc_tr, epochs = 30, batch_size = 128, 
      validation_split = 0.2)

# Test the model

 loss_deep <- model %>%
          evaluate(basis_te, soc_te)


