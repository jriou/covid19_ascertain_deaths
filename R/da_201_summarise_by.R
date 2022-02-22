#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: summarise samples
#:::::::::::::::::::::::::::::

da_201_summarise_by <- function(dat,by) {
  by = append(by,"it")
  summ = dat %>% 
    dplyr::group_by_at(vars(one_of(by))) %>% 
    dplyr::summarise(excess=sum(excess),
                     labo_deaths=sum(labo_deaths),
                     deaths=sum(deaths),
                     .groups="drop_last") %>%
    dplyr::summarise(excess_med=median(excess),
                     excess_lob=quantile(excess,0.025),
                     excess_upb=quantile(excess,0.975),
                     prob_above=mean(excess>labo_deaths),
                     labo_deaths=unique(labo_deaths),
                     deaths=unique(deaths),
                     .groups="drop") %>% 
    dplyr::mutate(metrics_current_excess=if_else(excess_lob>0,1,0),
                  metrics_coverage=if_else(labo_deaths<=excess_upb & labo_deaths>=excess_lob,1,0),
                  metrics_diff_med=excess_med-labo_deaths,
                  metrics_diff_lob=excess_lob-labo_deaths,
                  metrics_diff_upb=excess_upb-labo_deaths,
                  metrics_ascert_med=if_else(metrics_current_excess==1,labo_deaths/excess_med,NA_real_),
                  metrics_ascert_lob=if_else(metrics_current_excess==1,labo_deaths/excess_lob,NA_real_),
                  metrics_ascert_upb=if_else(metrics_current_excess==1,labo_deaths/excess_upb,NA_real_))
  
  return(summ)
}


