findtmax <- function (data){
  return(data %>% mutate(tmax = time[which(aif == max(aif))]))
}

slice_exp <- function(conc){
  conc = conc %>% 
    filter(time>=time[which(aif == max(aif))]) %>%
    mutate(t_G = t_G-time[which(aif == max(aif))],
           time = time-time[which(aif == max(aif))])
  return(conc)
}

slice_asc <- function(conc){
  conc = conc %>% 
    filter(time<=time[which(aif == max(aif))])
  return(conc)
}