#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: get excess
#:::::::::::::::::::::::::::::

da_104_get_excess <- function(dat) {

  # get indices
  dat2 = dat %>% 
    dplyr::mutate(id_loop = group_indices(., canton,age_group,sex,it_exp,it_pop)) %>% 
    dplyr::mutate(excess = deaths - exp_deaths,
                  mortality_rate = exp_deaths/population) 
  week.df = data.frame(week = unique(dat2$week)) %>% 
    mutate(week_nb = rank(week))
  dat2 = dat2 %>% 
    dplyr::left_join(., week.df,by="week") %>% 
    dplyr::arrange(id_loop,week_nb)
  
  # loop to compute shaved population and corrected expected deaths
  dat3 = list()
  n_weeks = dat2$week %>% unique() %>% length()
  
  curr = dat2 %>% 
    dplyr::filter(week_nb==1) %>% 
    dplyr::mutate(cumulative_excess = 0,
                  shaved_pop=population - cumulative_excess,
                  corr_exp_deaths=mortality_rate * shaved_pop,
                  corr_excess=deaths - corr_exp_deaths)
  cumu = dplyr::pull(curr,corr_excess)
  dat3[[1]] = curr
  
  for(i in 2:n_weeks){
    curr = dat2 %>% 
      dplyr::filter(week_nb==i) %>% 
      dplyr::mutate(cumulative_excess = cumu,
                    shaved_pop = population - cumulative_excess,
                    corr_exp_deaths = mortality_rate * shaved_pop,
                    corr_excess = deaths - corr_exp_deaths)
    cumu = cumu + dplyr::pull(curr,corr_excess)
    dat3[[i]] = curr
    print(i)
  }
  
  dat4 = data.table::rbindlist(dat3) %>% 
    as_tibble()
  
  if(FALSE) {
    dat4 %>% 
      dplyr::filter(canton_name=="Bern / Berne",
             age_group=="80+",
             sex=="male",
             it_exp==1) %>% 
      ggplot() +
      geom_line(aes(x=week,y=population,colour=factor(it_pop))) +
      geom_line(aes(x=week,y=shaved_pop,colour=factor(it_pop)),linetype=2)
    
    dat4 %>% 
      dplyr::filter(canton_name=="Bern / Berne",
                    age_group=="80+",
                    sex=="female",
                    it_exp==1) %>% 
      ggplot() +
      geom_line(aes(x=week,y=exp_deaths,colour=factor(it_pop))) +
      geom_line(aes(x=week,y=corr_exp_deaths,colour=factor(it_pop)),linetype=2)
    
    dat4 %>% 
      dplyr::filter(canton_name=="Bern / Berne",
                    age_group=="80+",
                    sex=="female",
                    it_exp==1) %>% 
      ggplot() +
      geom_line(aes(x=week,y=excess,colour=factor(it_pop))) +
      geom_line(aes(x=week,y=corr_excess,colour=factor(it_pop)),linetype=2)
  }
  
  return(dat4)
}


