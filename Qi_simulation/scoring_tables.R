
icr_inla <- sum( y>apply(pred_inla, 1,quantile, 0.025) & y<apply(pred_inla, 1,quantile, 0.975) )/length(y)
icr_dnn <- sum( y>apply(pred_dnn, 1,quantile, 0.025) & y<apply(pred_dnn, 1,quantile, 0.975) )/length(y)
icr_dk <- sum( y>apply(pred_dk, 1,quantile, 0.025) & y<apply(pred_dk, 1,quantile, 0.975) )/length(y)
icr_ck <- sum( y>apply(pred_ck, 1,quantile, 0.025) & y<apply(pred_ck, 1,quantile, 0.975) )/length(y)



score_tab <- 
rbind(
  
apply(loss_all, 2, mean)
,
-apply(cbind(crps_inla_all,crps_dnn_all,crps_dk_all,crps_ck_all),2, mean)
,
matrix(c(icr_inla, icr_dnn, icr_dk, icr_ck), nrow = 1)
,
apply(cbind(int_score_inla,int_score_dnn,int_score_dk,int_score_ck),2, mean)
)

rownames(score_tab) <- c("MSE", "Negative CRPS", "ICR", "Interval Score")
colnames(score_tab) <- c("INLA","Base DNN", "DK","CK")


xtable::xtable(t(round(score_tab,2)),aline = "c")

