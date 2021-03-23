scope_whole <- function(count,x1, x2,legend = TRUE){
  data = count$count[[1]] %>% filter(time <= x2 & time >= x1)
  asc_pred = count$asc_pred[[1]] %>% filter(time <= x2 & time >= x1)
  dsc_pred = count$dsc_pred[[1]] %>% filter(time <= x2 & time >= x1)
  patient = count$patient
  
  plot(data$time,data$aif, lty = 1,type="p",pch = 15,col = "grey",cex=0.5, main = paste0("patient:",patient),ylim = c(0,max(data$aif,asc_pred$pred,dsc_pred$pred)))
  lines(asc_pred$time, asc_pred$pred, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.8),pch = 20,type = "l",lwd = 1)
  lines(dsc_pred$time, dsc_pred$pred, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.8),pch = 20,type = "l",lwd = 1)
  if(legend){
    legend(x = "topright", legend=c("AIF","Poisson"),col=c("grey","red"), lty=c(NA,1,1),pch=c(20,NA,NA), cex=0.8)}
}

plot_rsd <-function(data,x1, x2){
  patient = data$patient
  rsd = data$rsd[[1]]
  rsd = rsd %>% 
    filter(time >= x1 & time <= x2) 
  # plot residual
  par(mfrow=c(1,2))
  plot(rsd$time,rsd$residual,pch = 20, cex = 1,col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5),main = paste0("patient:",patient))
  abline(h=0,lty = 2,lwd = 0.8)
  
  # box plot of residual with median
  boxplot(rsd$residual)
  abline(h=median(rsd$residual),col = "red")
  text(1 - 0.4, median(rsd$residual), 
       labels = formatC(median(rsd$residual), format = "f", 
                        digits = 1),
       pos = 3, cex = 0.9, col = "red")
}
