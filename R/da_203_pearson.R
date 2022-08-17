

da_203_pearson <- function(dat,by) {
  by = append(by,"it")
  summ = dat %>% 
    dplyr::group_by_at(vars(one_of(by))) %>% 
    dplyr::summarise(deaths=sum(deaths),
                     exp_deaths=sum(exp_deaths),
                     excess=sum(excess),
                     labo_deaths=sum(labo_deaths),
                     .groups="drop") %>% 
      dplyr::group_by(it) %>% 
      dplyr::summarise(pearson=cor(excess,labo_deaths),
                .groups="drop") %>% 
      dplyr::summarise(pearson_med=median(pearson),
                       pearson_lob=quantile(pearson,.025),
                       pearson_upb=quantile(pearson,.975))
      
  return(summ)
}


