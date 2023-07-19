score_tab <- 
  rbind(
    -apply(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all),2, mean)
    ,
    apply(cbind(int_score_inla,int_score_dnn,int_score_dk,int_score_ck),2, mean)
    ,
    apply(loss_mse,2, mean)
  )

rownames(score_tab) <- c("Negative CRPS","Interval Score","MSE")
colnames(score_tab) <- c("INLA","Base DNN", "DK","CK")

xtable::xtable(round(score_tab,2),aline = "c")


