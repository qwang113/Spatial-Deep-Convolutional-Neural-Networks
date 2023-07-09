
# Basis example ------------------------------------------

res_2_loc <- sapply(gridbasis2@pars, function(x) x$loc)
res_3_loc <- sapply(gridbasis3@pars, function(x) x$loc)[,-(1:ncol(res_2_loc))]

ggplot() +
  geom_raster( aes(x = res_3_loc[1,], y = res_3_loc[2,], fill = as.vector(t(basis_arr_3[10000,,])))) +
  labs(x = "Longitude", y = "Latitude", fill = "Basis Function") + 
  scale_fill_viridis() +
  geom_point(aes(x = long[10000], y = lat[10000]), col = "blue") +
  geom_point(aes(x = res_3_loc[1,], y = res_3_loc[2,]), col = "red", size = -250.5) +
  theme(legend.justification = "center")


# EH Shape ------------------------------------------

long_grid = lat_grid <- seq(from = -500, to = 500, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = y)) +
  scale_fill_viridis() +
  labs(x = "Longitude", y = "Latitude", fill = "Y")


# uncertainty with empty block------------------------------------------------------------------


long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, sd))) +
  scale_fill_viridis() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, sd))) +
  scale_fill_viridis()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, sd))) +
  scale_fill_viridis()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, sd))) +
  scale_fill_viridis() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 

# mean with empty block------------------------------------------------------------------


long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, mean))) +
  scale_fill_viridis() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, mean))) +
  scale_fill_viridis()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, mean))) +
  scale_fill_viridis()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, mean))) +
  scale_fill_viridis() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = -250.5, linetype = "dashed") 


