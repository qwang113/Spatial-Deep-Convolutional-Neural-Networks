
# Basis example ------------------------------------------

res_2_loc <- sapply(gridbasis2@pars, function(x) x$loc)
res_3_loc <- sapply(gridbasis3@pars, function(x) x$loc)[,-(1:ncol(res_2_loc))]

ggplot() +
  geom_raster( aes(x = res_3_loc[1,], y = res_3_loc[2,], fill = as.vector(t(basis_arr_3[10000,,])))) +
  labs(x = "Longitude", y = "Latitude", fill = "Basis Function") + 
  scale_fill_viridis() +
  geom_point(aes(x = long[10000], y = lat[10000]), col = "blue") +
  geom_point(aes(x = res_3_loc[1,], y = res_3_loc[2,]), col = "red", size = 0.5) +
  theme(legend.justification = "center")


# EH Shape ------------------------------------------

ggplot() +
  geom_raster(aes(x = long, y = lat, fill = y)) +
  scale_fill_viridis() +
  labs(x = "Longitude", y = "Latitude", fill = "Y")
