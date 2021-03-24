# get aif prediction based on gnm model results
# input model result and data after tmax
pred_aif <- function(mod_res, data){
    est_ac = coef(mod_res) # get coefficients from model results
    # calculate predicted aif using tri-exponential formula sum_i{exp(a_i+c_i*t})
    pred_conc = exp(est_ac[1]+est_ac[2]*data$time)+exp(est_ac[3]+est_ac[4]*data$time)+ exp(est_ac[5]+est_ac[6]*data$time)
    pred = tibble(time = data$time +unique(data$tmax), 
                  pred = pred_conc)
  return(pred)
}