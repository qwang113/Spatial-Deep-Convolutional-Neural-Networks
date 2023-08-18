mse_all <- read.csv(here::here("chen_simulation/mse_all.csv"))
apply(mse_all, 2, mean)
ckrig_pred <- read.csv(here::here("chen_simulation/ckrig_grid_pred.csv"))
dkrig_pred <- read.csv(here::here("chen_simulation/dkrig_grid_pred.csv"))
krig_pred <- read.csv(here::here("chen_simulation/krig_grid_pred.csv"))
mkrig_pred <- read.csv(here::here("chen_simulation/mkrig_grid_pred.csv"))
nn_pred <- read.csv(here::here("chen_simulation/nn_grid_pred.csv"))
show_idx <- 16
sim_dat <- read.csv(here::here("pm_small/sim_dat.csv"))[,-1]

ggplot() +
  geom_path(aes(x = seq(from = 0, to = 1, length.out = 200), y = as.numeric(krig_pred[show_idx,]), color = "Exp Kriging")) +
  geom_path(aes(x = seq(from = 0, to = 1, length.out = 200), y = as.numeric(nn_pred[show_idx,]), color = "DNN")) +
  geom_path(aes(x = seq(from = 0, to = 1, length.out = 200), y = as.numeric(dkrig_pred[show_idx,]), color = "Deep Kriging"))+ 
  geom_path(aes(x = seq(from = 0, to = 1, length.out = 200), y = as.numeric(mkrig_pred[show_idx,]), color = "Matern Kriging"))+ 
  geom_path(aes(x = seq(from = 0, to = 1, length.out = 200), y = as.numeric(ckrig_pred[show_idx,]), color = "Convolutional Kriging"))+
  geom_point( aes(x = seq(from = 0, to = 1, length.out = 1000), y = as.numeric(sim_dat[show_idx,])), size = 0.5) +
  labs(x = "S", y = "Z") 
