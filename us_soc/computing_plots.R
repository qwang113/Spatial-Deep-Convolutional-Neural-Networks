library(ggplot2)

us_soc <- read.csv(here::here("us_soc/soc_2.csv"))
long <- us_soc$long
lat <- us_soc$lat
y <- us_soc$soc
soc_dat <- na.omit(as.data.frame(cbind(long, lat, y)))
long <- soc_dat$long
lat <- soc_dat$lat
y <- soc_dat$y

grid_res <- 200

grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

g_long <- expand.grid(grid_long, grid_lat)[,1]
g_lat <- expand.grid(grid_long,grid_lat)[,2]
# Mean surface -----------------------------------------------------------------


pred_dnn <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/dnn_pred_soc.csv"))
pred_dk <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/dk_pred_soc.csv"))
pred_ck <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/ck_pred_soc.csv"))
pred_inla <- as.matrix(read.csv("D:/77/Research/temp/soc_pred//inla_pred_soc.csv"))

pred_dnn_g <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/dnn_pred_soc_g.csv"))
pred_dk_g <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/dk_pred_soc_g.csv"))
pred_ck_g <- as.matrix(read.csv("D:/77/Research/temp/soc_pred/ck_pred_soc_g.csv"))
pred_inla_g <- as.matrix(read.csv("D:/77/Research/temp/soc_pred//inla_pred_soc_g.csv"))

crps_dnn_all <- matrix(read.csv("D:/77/Research/temp/soc_pred/crps_dnn_soc.csv")$V1, ncol = 1)
crps_dk_all <- matrix(read.csv("D:/77/Research/temp/soc_pred/crps_dk_soc.csv")$V1, ncol = 1)
crps_ck_all <- matrix(read.csv("D:/77/Research/temp/soc_pred/crps_ck_soc.csv")$V1, ncol = 1)
crps_inla_all <- matrix(read.csv("D:/77/Research/temp/soc_pred/crps_inla_soc.csv")$x, ncol = 1)

int_score_dnn <- matrix(read.csv("D:/77/Research/temp/soc_pred/int_dnn_soc.csv")$x, ncol = 1)
int_score_dk <- matrix(read.csv("D:/77/Research/temp/soc_pred/int_dk_soc.csv")$x, ncol = 1)
int_score_ck <- matrix(read.csv("D:/77/Research/temp/soc_pred/int_ck_soc.csv")$x, ncol = 1)
int_score_inla <- matrix(read.csv("D:/77/Research/temp/soc_pred/int_inla_soc.csv")$x, ncol = 1)

rg = c(min(y)- sd(y),max(y)+sd(y))
ly <- log(y + 0.02)
rg_obs <- c(min(ly), max(ly))

# Observations ---------------------------------------------------------------
us_map <- map_data("usa")
obs_sur <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=long,y=lat,colour=ly),size=1,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg_obs, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "Observed Logarithm of Soil Organic Carbon content") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1.1) +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))

obs_sur
# mean -------------------------------------------------------------------------------------


tem = spBayes::pointsInPoly(as.matrix(map_data("usa", region = "main")[,1:2]),cbind(g_long, g_lat))


pred_sur_dnn <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_dnn_g,1,mean)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "Baseline FNN Prediction Surface") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  coord_fixed(ratio = 1.1) +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))

pred_sur_dk <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_dk_g,1,mean)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DeepKriging Prediction Surface") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50)) +
  coord_fixed(ratio = 1.1)

pred_sur_ck <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_ck_g,1,mean)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "SDCNN Prediction Surface") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

pred_sur_inla <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_inla_g,1,mean)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "INLA Prediction Surface") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

cowplot::plot_grid(pred_sur_inla, pred_sur_dnn, pred_sur_dk, pred_sur_ck)

# uncertainty --------------------------------------------------------------------------------
# rg <-c(0,
max(
    c(apply(pred_dnn_g,1,sd), apply(pred_dk_g,1,sd), apply(pred_ck_g,1,sd), apply(pred_inla_g,1,sd))
  )
# )
rg <- c(0,20)
sd_sur_dnn <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_dnn_g,1,sd)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(base) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

sd_sur_dk <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_dk_g,1,sd)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(basis) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

sd_sur_ck <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_ck_g,1,sd)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "CNN(basis) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

sd_sur_inla <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long[tem],y=g_lat[tem],colour=apply(pred_inla_g,1,sd)[tem]),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "INLA Posterior Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(c(-125, -67)) +
  ylim(c(25, 50))+
  coord_fixed(ratio = 1.1)

cowplot::plot_grid(sd_sur_inla, sd_sur_dnn, sd_sur_dk, sd_sur_ck)



# Scores (crps)------------------------------------------------------------------

library(ggforce)
ggplot(data = reshape2::melt(as.data.frame(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all))), 
       aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(labels = c("INLA", "DNN(base)", "DNN(basis)","CNN(basis)")) +
  scale_fill_manual(values = c("V1" = "lightblue", "V2" = "lightpink", "V3" = "lightgreen", "V4" = "lightyellow"),
                    labels = c("INLA", "DNN(base)", "DNN(basis)", "CNN(basis)")) +
  labs(fill = "Model", y = "Negative CRPS") + 
  facet_zoom(ylim = c(0,10)) +
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
  facet_zoom(ylim = c(0,300)) +
  guides(fill = "none")+
  theme_classic()


