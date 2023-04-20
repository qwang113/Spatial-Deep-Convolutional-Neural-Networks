llh_matern <- function(phi){
  
  out <- mvtnorm::dmvnorm(curr_sim_y, mean = rep(1, length(curr_sim_y)), 
                          sigma = Matern(spDists(cbind(1,s)), phi = phi, kappa = 1.5) )
  
}