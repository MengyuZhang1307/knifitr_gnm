pred_aif <- function(mod_res, data){
    est_ac = coef(mod_res)
    pred_conc = exp(est_ac[1]+est_ac[2]*data$time)+exp(est_ac[3]+est_ac[4]*data$time)+ exp(est_ac[5]+est_ac[6]*data$time)
    pred = tibble(time = data$time +unique(data$tmax), 
                  pred = pred_conc)
  return(pred)
}

# plot_pred_aif <- function(data, pred){
#   plot(data$time,data$aif, lty = 1,type="b",pch = 20, cex=1, main=paste("patient: ",i))
#   lines(data$time,pred,col = "red",pch = 20,type="b")
#   lines(data$time,data$kinfitr_aif,col = "blue", type = "b")
#   legend(x = "topright", legend=c("AIF","Predicted AIF","Kinfitr AIF"),
#          col=c("black","red","blue"), lty = 1, cex=0.8)
# }