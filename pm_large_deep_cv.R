rm(list = ls())
library(FRK)
library(spNNGP)
library(ggplot2)
library(maps)
library(MBA)
library(fields)
library(sp)
library(ncdf4)
library(reticulate)
library(tensorflow)
library(keras)
library(foreach)
library(foreach)
library(doParallel)
# Read the data

pm_large_all <- read.csv(here::here("annual_conc_by_monitor_2022.csv"))
pm_large_all <- pm_large_all[which(pm_large_all$Parameter.Name == "PM2.5 - Local Conditions"),]
point_aba <- unique(c(which(pm_large_all$Longitude <= -130), which(pm_large_all$Latitude <=20 ) ))
pm_large_all <- pm_large_all[-point_aba,]


long <- pm_large_all$Longitude + rnorm( length(pm_large_all$Longitude), 0 , sd = 0.001)
lat <- pm_large_all$Latitude + rnorm( length(pm_large_all$Latitude), 0 , sd = 0.001)
pm_filt <- pm_large_all$Arithmetic.Mean

pm_large <- as.data.frame(cbind(long, lat, pm_filt))
coordinates(pm_large) <- ~ long + lat
coords <- cbind(long, lat)

# Generate the basis function

gridbasis1 <- auto_basis(mainfold = plane(), data = pm_large, nres = 1, type = "Gaussian", regular = 1)
gridbasis2 <- auto_basis(mainfold = plane(), data = pm_large, nres = 2, type = "Gaussian", regular = 1)
gridbasis3 <- auto_basis(mainfold = plane(), data = pm_large, nres = 3, type = "Gaussian", regular = 1)


basis_1 <- matrix(NA, nrow = nrow(pm_large), ncol = length(gridbasis1@fn))
for (i in 1:length(gridbasis1@fn)) {
  basis_1[,i] <- gridbasis1@fn[[i]](coordinates(pm_large))
}

basis_2 <- matrix(NA, nrow = nrow(pm_large), ncol = length(gridbasis2@fn))
for (i in 1:length(gridbasis2@fn)) {
  basis_2[,i] <- gridbasis2@fn[[i]](coordinates(pm_large))
}

basis_3 <- matrix(NA, nrow = nrow(pm_large), ncol = length(gridbasis3@fn))
for (i in 1:length(gridbasis3@fn)) {
  basis_3[,i] <- gridbasis3@fn[[i]](coordinates(pm_large))
}


basis_use <- basis_3[,-(1:ncol(basis_2))]

# Clean the array

depth <- 3
row_shape <- length(table(gridbasis3@df[which(gridbasis3@df$res == depth) , 2 ]))
col_shape <- length(table(gridbasis3@df[which(gridbasis3@df$res == depth) , 1 ]))
basis_arr_all <- array(NA, dim = c(nrow(pm_large), row_shape, col_shape))
for (i in 1:nrow(pm_large)) {
  basis_arr_all[i,,] <- matrix(basis_use[i,], nrow = row_shape, ncol = col_shape, byrow = T)
}

# Create the cross validation index
set.seed(0)
cv_index_all <- sample(1:5, size = nrow(pm_large_all), replace = TRUE)
index_num <- table(cv_index_all)

pm_large_deep_cv <- function(curr_index, cv_index = cv_index_all, basis_arr = basis_arr_all,  pm_filt = pm_large_all$Arithmetic.Mean,
                             shape_row = row_shape, shape_col = col_shape){
  
  # Split the dataset to train and test 
  test_index <- which(cv_index == curr_index)
  basis_tr <- basis_arr[-test_index,,]
  basis_te <- basis_arr[test_index,,]
  
  # So we want to reshape each observation from a picture to a vector
  x_tr <- array_reshape(basis_tr, c(nrow(basis_tr), shape_row*shape_col))  
  # Same as prervious step
  x_te <- array_reshape(basis_te, c(nrow(basis_te), shape_row*shape_col))  
  pm_tr <- pm_filt[-test_index]
  pm_te <- pm_filt[test_index]
  
  
  model_dk <- keras_model_sequential() 
  
  model_dk %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(ncol(x_tr))) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 1, activation = 'linear')
  
  # Compile the model
  
  model_dk %>% keras::compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mse")
  )
  
  mod_train_dk <- model_dk %>%
    keras::fit(x = x_tr, y = pm_tr, epochs = 30, batch_size = 128)
  
  curr_predict <- predict(model_dk, x_te)
  curr_mse <- (pm_te - curr_predict)^2
  return(curr_mse)
}
  
cl <- makeCluster(5)
clusterEvalQ(cl, {
  library(FRK)
  library(spNNGP)
  library(ggplot2)
  library(maps)
  library(MBA)
  library(fields)
  library(sp)
  library(ncdf4)
  library(reticulate)
  library(tensorflow)
  library(keras)
  library(foreach)
  library(foreach)
  library(doParallel)
})

cv_mse_dl_para <- clusterApply(cl = cl, 1:5, fun = pm_large_deep_cv, cv_index = cv_index_all, basis_arr = basis_arr_all, 
                       pm_filt = pm_large_all$Arithmetic.Mean, shape_row = row_shape, shape_col = col_shape)


timepara <- system.time( clusterApply(cl = cl, 1:5, fun = pm_large_deep_cv, cv_index = cv_index_all, basis_arr = basis_arr_all, 
                                pm_filt = pm_large_all$Arithmetic.Mean, shape_row = row_shape, shape_col = col_shape))
stopCluster(cl)

timefore <- system.time( foreach(i = 1:5, .combine = "c") %dopar% pm_large_deep_cv(i))

cv_mse_dl_fore <- foreach(i = 1:5, .combine = "c") %dopar% pm_large_deep_cv(i)

time_all <- rbind(timepara,timefore)
