library(INLA)
library(rSPDE)
library(gridExtra)
library(lattice)

s_long <- ( g_long - min(long) )/diff(range(long)) * 10
s_lat <- (g_lat- min(lat))/diff(range(lat)) * 10

for (curr_index in 1:num_fold) {
  train_index <- which(train_index_all != curr_index)
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



# Prediction for grid
Aprediction <- inla.spde.make.A(mesh = mesh4, loc = cbind(s_long,s_lat));

stk.pred <- inla.stack(data=list(y=NA), 
                       A=list(Aprediction,1), 
                       effects=list(c(list(Intercept=1)
                                      ,iset),
                                    list(
                                      long = s_long,
                                      lat = s_lat
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
pred_inla_g <- matrix(sapply(temp_pos_sample, function(lst) lst$latent[index.pred]), ncol = num_sample)




