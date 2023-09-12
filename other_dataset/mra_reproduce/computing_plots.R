mra_dat <- unique(read.csv(here::here("other_dataset/mra_reproduce/MIRSmra.csv"),header = F))
colnames(mra_dat) <- c("long","lat","y")
mra_dat <- aggregate(y~long + lat, data = mra_dat, FUN = mean)
long <- mra_dat$long
lat <- mra_dat$lat
y <- mra_dat$y

grid_res <- 200
grid_long <- seq(from = min(long), to = max(long), length.out = grid_res)
grid_lat <- seq(from = min(lat), to = max(lat), length.out = grid_res)

gp_long <- expand.grid(grid_long, grid_lat)[,1]
g_long <- gp_long -180
g_lat <- expand.grid(grid_long,grid_lat)[,2]  
long <- mra_dat$long-180
# Mean surface -----------------------------------------------------------------


pred_dnn <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/dnn_pred_mra.csv"))
pred_dk <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/dk_pred_mra.csv"))
pred_ck <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/ck_pred_mra.csv"))
pred_inla <- as.matrix(read.csv("D:/77/Research/temp/mra_pred//inla_pred_mra.csv"))

pred_dnn_g <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/dnn_pred_mra_g.csv"))
pred_dk_g <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/dk_pred_mra_g.csv"))
pred_ck_g <- as.matrix(read.csv("D:/77/Research/temp/mra_pred/ck_pred_mra_g.csv"))
pred_inla_g <- as.matrix(read.csv("D:/77/Research/temp/mra_pred//inla_pred_mra_g.csv"))

crps_dnn_all <- matrix(read.csv("D:/77/Research/temp/mra_pred/crps_dnn_mra.csv")$x, ncol = 1)
crps_dk_all <- matrix(read.csv("D:/77/Research/temp/mra_pred/crps_dk_mra.csv")$x, ncol = 1)
crps_ck_all <- matrix(read.csv("D:/77/Research/temp/mra_pred/crps_ck_mra.csv")$x, ncol = 1)
crps_inla_all <- matrix(read.csv("D:/77/Research/temp/mra_pred/crps_inla_mra.csv")$x, ncol = 1)

int_score_dnn <- matrix(read.csv("D:/77/Research/temp/mra_pred/int_dnn_mra.csv")$x, ncol = 1)
int_score_dk <- matrix(read.csv("D:/77/Research/temp/mra_pred/int_dk_mra.csv")$x, ncol = 1)
int_score_ck <- matrix(read.csv("D:/77/Research/temp/mra_pred/int_ck_mra.csv")$x, ncol = 1)
int_score_inla <- matrix(read.csv("D:/77/Research/temp/mra_pred/int_inla_mra.csv")$x, ncol = 1)

# rg <- range(c(apply(pred_dnn_g,1,mean), apply(pred_dk_g,1, mean), apply(pred_ck_g,1, mean),apply(pred_inla_g,1, mean)))
# rg[1] <- 0

rg <- range(y) + c(-sd(y), sd(y))

# Observations ---------------------------------------------------------------
# borders <- map_data("world")
  # ggplot() +
  # geom_point(aes(x = long, y = lat, fill = y)) +
  # scale_fill_viridis_c(limits = c(min(y), max(y))) +
  # labs(x = "Longitude", y = "Latitude", fill = "Y", title = "Observation Surface") +
  # theme_bw() +
  # theme(plot.title = element_text(hjust = 0.5)) 
  # geom_path(data = borders, aes(x = long, y = lat, group = group), color = "red") 
  # 
borders <- map_data("world")[which( map_data("world")$region=="USA" ),]

obs_sur <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=long,y=lat,colour=y),size=2,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "red") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "Observation Points") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(long)) +
  ylim(range(lat))
  
obs_sur

# mean -------------------------------------------------------------------------------------
pred_sur_dnn <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_dnn_g,1,mean)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(base) Mean Prediction") + 
  theme(plot.title = element_text(hjust = 0.5))  +
  xlim(range(long)) +
  ylim(range(lat))


pred_sur_dk <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_dk_g,1,mean)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(basis) Mean Prediction") + 
  theme(plot.title = element_text(hjust = 0.5))    +
  xlim(range(long)) +
  ylim(range(lat))


pred_sur_ck <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_ck_g,1,mean)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "CNN(basis) Mean Prediction") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 


pred_sur_inla <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_inla_g,1,mean)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "INLA Posterior Mean Prediction") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 


cowplot::plot_grid(pred_sur_inla, pred_sur_dnn, pred_sur_dk, pred_sur_ck)

# uncertainty --------------------------------------------------------------------------------
# rg <-c(0,
max(
  c(apply(pred_dnn_g,1,sd), apply(pred_dk_g,1,sd), apply(pred_ck_g,1,sd), apply(pred_inla_g,1,sd))
)
# )
rg <- c(0,3)

sd_sur_dnn <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_dnn_g,1,sd)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(base) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 


sd_sur_dk <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_dk_g,1,sd)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "DNN(basis) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 

sd_sur_ck <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_ck_g,1,sd)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "CNN(basis) Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 

sd_sur_inla <-
  ggplot() + theme_bw() + 
  theme(legend.title=element_blank(),legend.key.width=unit(2,'cm'),legend.position='bottom') +
  geom_point(aes(x=g_long,y=g_lat,colour=apply(pred_inla_g,1,sd)),size=1.5,shape=19) + 
  scale_colour_gradientn(colours = hcl.colors(10),limits=rg, na.value = "red") +
  geom_path(data = borders, aes(x = long, y = lat, group = group), color = "white") +
  # scale_x_continuous(limits=range(long),expand=c(0,0)) + 
  # scale_y_continuous(limits=range(lat),expand=c(0,0)) +
  labs(x = "Longitude", y = "Latitude", title = "INLA Posterior Standard Deviation") + 
  theme(plot.title = element_text(hjust = 0.5))   +
  xlim(range(long)) +
  ylim(range(lat)) 


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
  facet_zoom(ylim = c(0,10)) +
  guides(fill = "none")+
  theme_classic()


