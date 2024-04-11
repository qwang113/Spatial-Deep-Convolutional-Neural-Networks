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
  out <- -(500*y + 47)*sin(sqrt(abs(500*x/2 + 500*y + 47))) - 500*x*sin(sqrt(abs(500*x-(500*y + 47))))
}

long_grid = lat_grid <- seq(from = -1, to = 1, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]
eh_dat <- data.frame(long = long, lat = lat, y = y)  

coordinates(eh_dat) <- ~ long + lat

# Parameters setting
set.seed(0)
num_sample <- 100
pred_drop <- 0.1
num_fold <- 5

# pred_dnn <- array(NA, dim = c(num_sample, num_fold, floor(length(y)*0.1)))
# pred_dk <- matrix(NA,nrow = length(y), ncol = num_sample)
# pred_ck <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_inla <- array(NA, dim = c(num_sample, num_fold, floor(length(y)*0.1)))

# Basis Generating
library(INLA)
library(rSPDE)
library(gridExtra)
library(lattice)
library(geoR)

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


long_s <- scale(long)
lat_s <- scale(lat)


inla_range <- min( diff(range(long)), diff(range(lat)) ) / sqrt(length(long)) * 5

set.seed(0)
eh_dat <- data.frame(long = long, lat = lat, y = y)  
tr_idx <- sample(1:nrow(eh_dat),floor(nrow(eh_dat)*0.9))
eh_tr <- eh_dat[tr_idx,]
eh_te <- eh_dat[-tr_idx,]
train_index_all <- sample(1:num_fold, nrow(eh_tr), replace = T)
for (curr_index in 1:num_fold) {
  
  train_index <- which(train_index_all != curr_index)
  
  long_tr <- long[tr_idx[train_index]]
  lat_tr <- lat[tr_idx[train_index]]
  y_tr <- y[tr_idx[train_index]]
  coords <- cbind(long_tr, lat_tr)
  
  print("Building mesh . . .")
  
  non_convex_bdry <- inla.nonconvex.hull(coords, -0.3, resolution = c(100, 100))
  mesh4 <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(inla_range, inla_range*2))
  
  
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
  
  print("Fitting model . . .")
  
  model0<-inla(formula0, #the formula
               data=inla.stack.data(stk,spde=spde),  #the data stack
               family= 'gaussian',   #which family the data comes from
               control.predictor=list(A=inla.stack.A(stk),compute=TRUE),  #compute gives you the marginals of the linear predictor
               control.compute = list(dic = TRUE, waic = TRUE, config = TRUE, return.marginals.predictor = TRUE), #model diagnostics and config = TRUE gives you the GMRF
               verbose = FALSE) #can include verbose=TRUE to see the log of the model runs
  
  
  
  long_te <- long[-tr_idx]
  lat_te <- lat[-tr_idx]
  y_te <- y[-tr_idx]
  
  
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
  print("Making posterior predictions . . .") 
  p.res.pred<-inla(formula0, data=inla.stack.data(stk.full,spde=spde),
                   family= 'gaussian', quantiles = NULL,
                   control.predictor=list(link = 1, A=inla.stack.A(stk.full),compute=TRUE),  #compute gives you the marginals of the linear predictor
                   control.compute = list(config = TRUE), #model diagnostics and config = TRUE gives you the GMRF
                   control.inla = list(strategy = 'simplified.laplace', huge = TRUE),  #this is to make it run faster
                   verbose = FALSE) 
  
  index.pred<-inla.stack.index(stk.full, "pred")$data
  
  # <- inla.posterior.sample(n = num_sample, p.res.pred)
  temp_pos_sample <- inla.posterior.sample(num_sample,p.res.pred)
  pred_inla[ , curr_index, ] <- t(matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample))
}

mean((apply(pred_inla[,1,], 2 , mean) - y[-tr_idx])^2)
saveRDS(pred_inla,"D:/77/Research/temp/eh_pred/inla_pred_eh.rds")
