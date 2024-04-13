rm(list = ls())
library(FRK)
library(spNNGP)
library(ggplot2)
library(maps)
library(MBA)
library(fields)
library(sp)
library(ncdf4)
library(geoR)

min_max_scale <- function(x)
{
  low <- range(x)[1]
  high <- range(x)[2]
  
  out <- (x - low)/(high - low)
  return(out)
  
}

load(here::here("other_dataset/satellite/AllSatelliteTemps.RData"))
sat_dat <- na.omit(all.sat.temps[,c(1,2,4)])
colnames(sat_dat) <- c("long","lat", "y")
long <- sat_dat$long
lat <- sat_dat$lat
y <- sat_dat$y


coordinates(sat_dat) <- ~ long + lat
grid_res <- 200
# Parameters setting

grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]

# Parameters setting
set.seed(0)
num_sample <- 100
num_fold <- 5


library(INLA)
library(rSPDE)
library(gridExtra)
library(lattice)
pred_inla <- matrix(NA,nrow = length(y), ncol = num_sample)
pred_inla_g <- matrix(NA, nrow = grid_res^2, ncol = num_sample)

inla_range <- min( diff(range(long)), diff(range(lat)) ) / sqrt(length(long)) * 10
fold_number <- sample(1:num_fold,nrow(sat_dat), replace = TRUE)

for (curr_index in 1:num_fold) {
  
  tr_idx <- which(fold_number != curr_index)
  te_idx <- which(fold_number == curr_index)
  train_index <- sample(1:length(tr_idx), floor(0.9*length(tr_idx)))
  long_tr <- long[train_index]
  lat_tr <- lat[train_index]
  y_tr <- y[train_index]
  long_te <- long[te_idx]
  lat_te <- lat[te_idx]
  coords <- cbind(long_tr, lat_tr)
  
  print("Building mesh . . .")
  
  non_convex_bdry <- inla.nonconvex.hull(coords, -0.3, resolution = c(200, 200))
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
               control.inla = list(strategy = 'simplified.laplace', huge = TRUE),
               verbose = FALSE) #can include verbose=TRUE to see the log of the model runs
  
  
  
  long_te <- long[te_idx]
  lat_te <- lat[te_idx]
  
  
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
  pred_inla[te_idx,] <- matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample)
  # Prediction for grid
  Aprediction <- inla.spde.make.A(mesh = mesh4, loc = cbind(g_long,g_lat))
  
  stk.pred <- inla.stack(data=list(y=NA),
                         A=list(Aprediction,1),
                         effects=list(c(list(Intercept=1)
                                        ,iset),
                                      list(
                                        long = g_long,
                                        lat = g_lat
                                      )
                         ),
                         tag='pred')
  
  #join the prediction stack with the one for the full data
  stk.full <- inla.stack(stk, stk.pred)
  
  p.res.pred<-inla(formula0, data=inla.stack.data(stk.full,spde=spde),
                   family= 'gaussian', quantiles = NULL,
                   control.predictor=list(link = 1, A=inla.stack.A(stk.full),compute=TRUE),  #compute gives you the marginals of the linear predictor
                   control.compute = list(config = TRUE), #model diagnostics and config = TRUE gives you the GMRF
                   control.inla = list(strategy = 'simplified.laplace', huge = TRUE),  #this is to make it run faster
                   verbose = FALSE)
  
  index.pred<-inla.stack.index(stk.full, "pred")$data
  
  # <- inla.posterior.sample(n = num_sample, p.res.pred)
  temp_pos_sample <- inla.posterior.sample(num_sample,p.res.pred)
  pred_inla_g <- matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample)
  
}

mean((apply(pred_inla, 1 , mean) - y)^2)
write.csv(as.data.frame(pred_inla),"D:/77/Research/temp/sat_pred/inla_pred_sat.csv",row.names = FALSE)
write.csv(as.data.frame(pred_inla_g),"D:/77/Research/temp/sat_pred/inla_pred_sat_g.csv",row.names = FALSE)
