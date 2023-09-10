library(ggplot2)

load(here::here("other_dataset/satellite/AllSatelliteTemps.RData"))
sat_dat <- na.omit(all.sat.temps[,c(1,2,4)])
colnames(sat_dat) <- c("long","lat", "y")
long <- sat_dat$long
lat <- sat_dat$lat
y <- sat_dat$y
grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]

# Observations ---------------------------------------------------------------
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = y)) +
  scale_fill_viridis_c(limits = c(min(y), max(y))) +
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "Observation Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Mean surface -----------------------------------------------------------------
library(ggplot2)

pred_dnn <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/dnn_pred_sat.csv"))
pred_dk <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/dk_pred_sat.csv"))
pred_ck <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/ck_pred_sat.csv"))
pred_inla <- as.matrix(read.csv("D:/77/Research/temp/sat_pred//inla_pred_sat.csv"))

pred_dnn_g <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/dnn_pred_sat_g.csv"))
pred_dk_g <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/dk_pred_sat_g.csv"))
pred_ck_g <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/ck_pred_sat_g.csv"))
pred_inla_g <- as.matrix(read.csv("D:/77/Research/temp/sat_pred/inla_pred_sat_g.csv"))


crps_dnn_all <- matrix(read.csv("D:/77/Research/temp/sat_pred/crps_dnn_sat.csv")$x, ncol = 1)
crps_dk_all <- matrix(read.csv("D:/77/Research/temp/sat_pred/crps_dk_sat.csv")$x, ncol = 1)
crps_ck_all <- matrix(read.csv("D:/77/Research/temp/sat_pred/crps_ck_sat.csv")$x, ncol = 1)
crps_inla_all <- matrix(read.csv("D:/77/Research/temp/sat_pred/crps_inla_sat.csv")$x, ncol = 1)

int_score_dnn <- matrix(read.csv("D:/77/Research/temp/sat_pred/int_dnn_sat.csv")$x, ncol = 1)
int_score_dk <- matrix(read.csv("D:/77/Research/temp/sat_pred/int_dk_sat.csv")$x, ncol = 1)
int_score_ck <- matrix(read.csv("D:/77/Research/temp/sat_pred/int_ck_sat.csv")$x, ncol = 1)
int_score_inla <- matrix(read.csv("D:/77/Research/temp/sat_pred/int_inla_sat.csv")$x, ncol = 1)



# mean -------------------------------------------------------------------------------------
pred_sur_dnn <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_dnn_g, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(base) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_dk <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_dk_g, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(basis) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_ck <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_ck_g, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "CNN(basis) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_inla <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_inla_g, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "INLA Posterior Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(pred_sur_inla, pred_sur_dnn, pred_sur_dk, pred_sur_ck)

# uncertainty --------------------------------------------------------------------------------

upper <- max(c(
  apply(pred_dnn, 1, sd),
  apply(pred_dk, 1, sd),
  apply(pred_ck, 1, sd),
  apply(pred_inla, 1, sd)
)) 

sd_sur_dnn <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_dnn_g, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN(base) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_dk <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_dk_g, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN(basis) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_ck <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_ck_g, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "CNN(basis) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_inla <-
  ggplot() +
  geom_raster(aes(x = g_long, y = g_lat, fill = apply(pred_inla_g, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "INLA Posterior Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(sd_sur_inla, sd_sur_dnn, sd_sur_dk, sd_sur_ck)


# Scores (crps)------------------------------------------------------------------ 

library(ggforce)
ggplot(data = reshape2::melt(as.data.frame(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all))), 
       aes(x = variable, y = -value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN(base)", "DNN(basis)","CNN(basis)")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN(base)", "DNN(basis)", "CNN(basis)")) +
  labs(fill = "Model", y = "Negative CRPS") + 
  facet_zoom(ylim = c(0,1.5)) +
  guides(fill = "none")+
  theme_classic()


# Scores (Interval)------------------------------------------------------------------

ggplot(data = reshape2::melt(as.data.frame(cbind(int_score_inla,int_score_dnn,int_score_dk, int_score_ck)))
       , aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN(base)", "DNN(basis)","CNN(basis)")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN(base)", "DNN(basis)", "CNN(basis)")) +
  labs(fill = "Model", y = "Interval Score") + 
  facet_zoom(ylim = c(0,40)) +
  guides(fill = "none")+
  theme_classic()

