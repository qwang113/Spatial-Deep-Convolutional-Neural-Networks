library(ggplot2)



pred_dnn <- as.matrix(read.csv("D:/77/Reasearch/temp/dnn_pred_eh.csv"))
pred_dk <- as.matrix(read.csv("D:/77/Reasearch/temp/dk_pred_eh.csv"))
pred_ck <- as.matrix(read.csv("D:/77/Reasearch/temp/ck_pred_eh.csv"))
pred_inla <- as.matrix(read.csv("D:/77/Reasearch/temp/inla_pred_eh.csv"))

crps_dnn_all <- matrix(read.csv("D:/77/Reasearch/temp/crps_dnn_eh.csv")$x, ncol = 1)
crps_dk_all <- matrix(read.csv("D:/77/Reasearch/temp/crps_dk_eh.csv")$x, ncol = 1)
crps_ck_all <- matrix(read.csv("D:/77/Reasearch/temp/crps_ck_eh.csv")$x, ncol = 1)
crps_inla_all <- matrix(read.csv("D:/77/Reasearch/temp/crps_inla_eh.csv")$x, ncol = 1)

int_score_dnn <- matrix(read.csv("D:/77/Reasearch/temp/int_dnn_eh.csv")$x, ncol = 1)
int_score_dk <- matrix(read.csv("D:/77/Reasearch/temp/int_dk_eh.csv")$x, ncol = 1)
int_score_ck <- matrix(read.csv("D:/77/Reasearch/temp/int_ck_eh.csv")$x, ncol = 1)
int_score_inla <- matrix(read.csv("D:/77/Reasearch/temp/int_inla_eh.csv")$x, ncol = 1)


pred_empty_area_dnn <- as.matrix(read.csv("D:/77/Reasearch/temp/dnn_pred_empty_area_eh.csv"))
pred_empty_area_dk <- as.matrix(read.csv("D:/77/Reasearch/temp/dk_pred_empty_area_eh.csv"))
pred_empty_area_ck <- as.matrix(read.csv("D:/77/Reasearch/temp/ck_pred_empty_area_eh.csv"))
pred_empty_area_inla <- as.matrix(read.csv("D:/77/Reasearch/temp/inla_pred_empty_area_eh.csv"))




long_grid = lat_grid <- seq(from = -500, to = 500, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]



# Basis example ------------------------------------------

res_2_loc <- sapply(gridbasis2@pars, function(x) x$loc)
res_3_loc <- sapply(gridbasis3@pars, function(x) x$loc)[,-(1:ncol(res_2_loc))]

ggplot() +
  geom_raster( aes(x = res_3_loc[1,], y = res_3_loc[2,], fill = as.vector(t(basis_arr_3[10000,,])))) +
  labs(x = "Longitude", y = "Latitude", fill = "Basis Function") + 
  scale_fill_viridis_c() +
  geom_point(aes(x = long[10000], y = lat[10000]), col = "blue") +
  geom_point(aes(x = res_3_loc[1,], y = res_3_loc[2,]), col = "red", size = 1) +
  theme(legend.justification = "center")


# EH Shape ------------------------------------------

long_grid = lat_grid <- seq(from = -500, to = 500, length.out = sim_size)

y <- as.vector(outer(X = long_grid, Y = lat_grid, FUN = Vectorize(egg_fun)))

long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


# mean -------------------------------------------------------------------------------------
obs_sur <- 
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = y)) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "True Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_dnn <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dnn, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN Mean Prediction Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_dk <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dk, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "Deep Kriging Mean Prediction Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_ck <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_ck, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "Convolutional Kriging Mean Prediction Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_inla <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_inla, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "INLA Posterior Mean Prediction Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# uncertainty --------------------------------------------------------------------------------


sd_sur_dnn <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dnn, 1, sd))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN Standard Deviation Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_dk <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dk, 1, sd))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "Deep Kriging Standard Deviation Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_ck <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_ck, 1, sd))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "Convolutional Kriging Standard Deviation Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_inla <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_inla, 1, sd))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "INLA Posterior Standard Deviation Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# uncertainty with empty block------------------------------------------------------------------


long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


sd_sur_dnn_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, sd))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN Standard Deviation Surface(Empty Block Added") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, sd))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "Deep Kriging Standard Deviation Surface(Empty Block Added") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, sd))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "Convolutional Kriging Standard Deviation Surface(Empty Block Added") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, sd))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "INLA Posterior Standard Deviation Surface(Empty Block Added") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# mean with empty block------------------------------------------------------------------


long <- expand.grid(long_grid, lat_grid)[,1]
lat <- expand.grid(long_grid, lat_grid)[,2]


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, mean))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, mean))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, mean))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") 


ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, mean))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(300,400),y = c(-250,-250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,400),y = c(250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(300,300),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(400,400),y = c(-250,250)), color = "red", linewidth = 1, linetype = "dashed") 


# Scores (crps)------------------------------------------------------------------

library(ggforce)
ggplot(data = reshape2::melt(as.data.frame(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all))), 
       aes(x = variable, y = -value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN", "DK","CK")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN", "DK", "CK")) +
  labs(fill = "Model", y = "Negative CRPS") + 
  facet_zoom(ylim = c(0,15)) +
  theme_classic()


# Scores (Interval)------------------------------------------------------------------

ggplot(data = reshape2::melt(as.data.frame(cbind(int_score_inla,int_score_dnn,int_score_dk, int_score_ck)))
       , aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN", "DK","CK")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN", "DK", "CK")) +
  labs(fill = "Model", y = "Interval Score") + 
  facet_zoom(ylim = c(0,1000)) +
  theme_classic()



  
