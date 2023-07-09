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
  out <- -(y + 47)*sin(sqrt(abs(x/2 + y + 47))) - x*sin(sqrt(abs(x-(y + 47))))
}

long_grid = lat_grid <- seq(from = -500, to = 500, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]



num_fold <- 5
num_sample <- 100
pred_drop <- 0.2

pred_drop_layer <- layer_dropout(rate=pred_drop)

set.seed(0)
train_index_all <- sample(1:num_fold, length(y), replace = T)
pred_empty_area_dnn <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_empty_area_dk <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_empty_area_ck <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_empty_area_inla <- matrix(NA,nrow = length(y), ncol = num_sample)

test_area_index <- which(long >= 500 & long <=800 & lat >= -500 & lat <=500 )

library(INLA)
library(rSPDE)
library(gridExtra)
library(lattice)
for (curr_index in 1:num_fold) {
  train_index_temp <- which(train_index_all != curr_index)
  train_index <- train_index_temp[!(train_index_temp %in% test_area_index)]
  
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