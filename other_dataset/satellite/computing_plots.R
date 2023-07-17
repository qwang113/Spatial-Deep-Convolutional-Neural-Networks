# Observations ---------------------------------------------------------------
ggplot() +
  geom_point(aes(x = long, y = lat, color = y), size = 0.005) +
  scale_color_viridis_c() +
  labs(x = "Longitude", y = "Latitude", color = "Y")

