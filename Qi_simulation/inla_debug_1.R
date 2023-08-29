rm(list = ls())
library(sp)
library(fields)
library(ggplot2)
library(INLA)
library(rSPDE)
library(gridExtra)
library(lattice)

min_max_scale <- function(x)
{
  low <- range(x)[1]
  high <- range(x)[2]
  
  out <- (x - low)/(high - low)
  return(out)
  
}

res <- 50
g_long <- seq(from = 0, to = 10, length.out = res)
g_lat <- seq(from = 0, to = 10, length.out = res)
long <- expand.grid(g_long,g_lat)[,1]
lat <- expand.grid(g_long,g_lat)[,2]
p_dist <- spDists(cbind(long, lat))
cov_fun <- exp(-(p_dist/2)^2)
cov_mat <- matrix(cov_fun, nrow = res^2, ncol = res^2)
y <- as.vector(mvtnorm::rmvnorm(1, mean = rep(0, res^2), sigma = cov_mat))


# obs_sur <- 
#   ggplot() +
#   geom_raster(aes(x = long, y = lat, fill = y)) +
#   scale_fill_viridis_c() + 
#   labs(x = "Longitude", y = "Latitude", fill = "Y", title = "True Surface") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) 


scale_param <- 1
icr_level = 0.05
  
set.seed(0)
num_sample = 1000
num_fold <- 5
train_index_all <- sample(1:num_fold, size = res^2, replace = TRUE)
pred_inla <- matrix(NA, nrow = res^2, ncol = num_sample)
loss_inla <- rep(NA, 5)


# INLA Model

for (curr_index in 1:num_fold) {
  
  print( paste("Now it's index ", curr_index ))
  
  train_index <- which(train_index_all != curr_index)
  long <- min_max_scale(long) * scale_param
  lat <- min_max_scale(lat) * scale_param
  long_tr <- long[train_index]
  lat_tr <- lat[train_index]
  y_tr <- y[train_index]
  coords <- cbind(long_tr, lat_tr)
  
  non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
  mesh4 <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(0.05, 0.1))
  
  
  
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
               verbose = FALSE) #can include verbose=TRUE to see the log of the model runs
  
  
  
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
  pred_inla[-train_index,] <- matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample)
  loss_inla[curr_index] <- mean((p.res.pred$summary.linear.predictor[index.pred,"mean"] - y_te)^2)
  
}

icr_inla <- sum( y>apply(pred_inla, 1,quantile, icr_level/2) & y<apply(pred_inla, 1,quantile, 1 - icr_level/2) )/length(y)


# # Performance
# p1 <-
#   ggplot() +
#   geom_raster(aes(x = long, y = lat, fill = y)) +
#   scale_fill_viridis_c() + 
#   labs(x = "Longitude", y = "Latitude", fill = "Y", title = "True Surface") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# p2 <-
#   ggplot() +
#   geom_raster(aes(x = long, y = lat, fill = apply(pred_inla, 1, mean))) +
#   scale_fill_viridis_c() + 
#   labs(x = "Longitude", y = "Latitude", fill = "Y", title = "INLA Surface") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# cowplot::plot_grid(p1,p2)

