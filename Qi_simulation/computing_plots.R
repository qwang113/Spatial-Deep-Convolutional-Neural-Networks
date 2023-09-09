library(ggplot2)

pred_dnn <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/dnn_pred_eh.csv"))
pred_dk <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/dk_pred_eh.csv"))
pred_ck <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/ck_pred_eh.csv"))
pred_inla <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/inla_pred_eh.csv"))

pred_empty_area_dnn <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/dnn_pred_empty_area_eh.csv"))
pred_empty_area_dk <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/dk_pred_empty_area_eh.csv"))
pred_empty_area_ck <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/ck_pred_empty_area_eh.csv"))
pred_empty_area_inla <- as.matrix(read.csv("D:/77/Research/temp/eh_pred/inla_pred_empty_area_eh.csv"))

crps_dnn_all <- matrix(read.csv("D:/77/Research/temp/eh_pred/crps_dnn_eh.csv")$x, ncol = 1)
crps_dk_all <- matrix(read.csv("D:/77/Research/temp/eh_pred/crps_dk_eh.csv")$x, ncol = 1)
crps_ck_all <- matrix(read.csv("D:/77/Research/temp/eh_pred/crps_ck_eh.csv")$x, ncol = 1)
crps_inla_all <- matrix(read.csv("D:/77/Research/temp/eh_pred/crps_inla_eh.csv")$x, ncol = 1)

int_score_dnn <- matrix(read.csv("D:/77/Research/temp/eh_pred/int_dnn_eh.csv")$x, ncol = 1)
int_score_dk <- matrix(read.csv("D:/77/Research/temp/eh_pred/int_dk_eh.csv")$x, ncol = 1)
int_score_ck <- matrix(read.csv("D:/77/Research/temp/eh_pred/int_ck_eh.csv")$x, ncol = 1)
int_score_inla <- matrix(read.csv("D:/77/Research/temp/eh_pred/int_inla_eh.csv")$x, ncol = 1)


sim_size = 300

egg_fun <- function(x,y){
  out <- -(500*y + 47)*sin(sqrt(abs(500*x/2 + 500*y + 47))) - 500*x*sin(sqrt(abs(500*x-(500*y + 47))))
}

long_grid = lat_grid <- seq(from = -1, to = 1, length.out = sim_size)

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


# EH Shape ---------------------------------------------------------------------------
obs_sur <- 
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = y)) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "True Surface") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

obs_sur_emp <- 
  ggplot() +
  geom_raster(aes(x = long[-test_area_index], y = lat[-test_area_index], fill = y[-test_area_index])) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "True Surface with Rectangle Block Subtracted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed")



# mean -------------------------------------------------------------------------------------
pred_sur_dnn <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dnn, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(base) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_dk <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dk, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(basis) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_ck <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_ck, 1, mean))) +
  scale_fill_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "CNN(basis) Mean Prediction") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_inla <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_inla, 1, mean))) +
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
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dnn, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN(base) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_dk <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_dk, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN(basis) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_ck <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_ck, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "CNN(basis) Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_inla <-
  ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_inla, 1, sd))) +
  scale_fill_viridis_c(limits = c(0, upper)) + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "INLA Posterior Standard Deviation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(sd_sur_inla, sd_sur_dnn, sd_sur_dk, sd_sur_ck)

# uncertainty with empty block------------------------------------------------------------------
test_area_index <- which(long >= 300/500 & long <=400/500 & lat >= -250/500 & lat <= 250/500 )

upper_emp <- max(c(
  apply(pred_empty_area_dnn, 1, sd),
  apply(pred_empty_area_dk, 1, sd),
  apply(pred_empty_area_ck, 1, sd),
  apply(pred_empty_area_inla, 1, sd)
)) 


sd_sur_dnn_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, sd))) +
  scale_fill_viridis_c(limits = c(0,upper_emp)) +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DNN Standard Deviation(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_dk_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, sd))) +
  scale_fill_viridis_c(limits = c(0,upper_emp)) +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "DK Standard Deviation(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_ck_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, sd))) +
  scale_fill_viridis_c(limits = c(0,upper_emp)) +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "CK Standard Deviation(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sd_sur_inla_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, sd))) +
  scale_fill_viridis_c(limits = c(0,upper_emp)) +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = "INLA Standard Deviation(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(sd_sur_inla_emp, sd_sur_dnn_emp, sd_sur_dk_emp, sd_sur_ck_emp)

# mean with empty block------------------------------------------------------------------
pred_sur_dnn_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dnn, 1, mean))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed")  +
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(base) Mean Prediction(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_dk_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_dk, 1, mean))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed")  +
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "DNN(basis) Mean Prediction(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_ck_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_ck, 1, mean))) +
  scale_fill_viridis_c()+
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed")  +
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "CNN(basis) Mean Prediction(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pred_sur_inla_emp <-
ggplot() +
  geom_raster(aes(x = long, y = lat, fill = apply(pred_empty_area_inla, 1, mean))) +
  scale_fill_viridis_c() +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),min(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),max(long[test_area_index]) + unique(diff(long))[1]),y = c(max(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(min(long[test_area_index]),min(long[test_area_index])),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(x = c(max(long[test_area_index]) + unique(diff(long))[1],max(long[test_area_index]) + unique(diff(long))[1]),y = c(min(lat[test_area_index]),max(lat[test_area_index]))), color = "red", linewidth = 1, linetype = "dashed")  +
  labs(x = "Longitude", y = "Latitude", fill = "Y", title = "INLA Posterior Mean Prediction(Block Subtracted)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(pred_sur_inla_emp, pred_sur_dnn_emp, pred_sur_dk_emp, pred_sur_ck_emp)

# Scores (crps)------------------------------------------------------------------

library(ggforce)
ggplot(data = reshape2::melt(as.data.frame(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all))), 
       aes(x = variable, y = -value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN(base)", "DNN(basis)","CNN(basis)")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN(base)", "DNN(basis)", "CNN(basis)")) +
  labs(fill = "Model", y = "Negative CRPS") + 
  facet_zoom(ylim = c(0,6)) +
  theme_classic()


# Scores (Interval)------------------------------------------------------------------

ggplot(data = reshape2::melt(as.data.frame(cbind(int_score_inla,int_score_dnn,int_score_dk, int_score_ck)))
       , aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN(base)", "DNN(basis)","CNN(basis)")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN(base)", "DNN(basis)", "CNN(basis)")) +
  labs(fill = "Model", y = "Interval Score") + 
  facet_zoom(ylim = c(0,75)) +
  theme_classic()



  
