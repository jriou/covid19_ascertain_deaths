#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: summarise samples
#:::::::::::::::::::::::::::::

da_202_summarise_by_corr <- function(dat,by) {
  by = append(by,"it")
  summ = dat %>% 
    dplyr::group_by_at(vars(one_of(by))) %>% 
    dplyr::summarise(deaths=sum(deaths),
                     exp_deaths=sum(corr_exp_deaths),
                     excess=sum(corr_excess),
                     rel_excess=if_else(exp_deaths>0,excess/exp_deaths,0),
                     labo_deaths=sum(labo_deaths),
                     .groups="drop_last") %>%
    dplyr::summarise(exp_deaths_med=median(exp_deaths),
                     exp_deaths_lob=quantile(exp_deaths,0.025),
                     exp_deaths_upb=quantile(exp_deaths,0.975),
                     excess_med=median(excess),
                     excess_lob=quantile(excess,0.025),
                     excess_upb=quantile(excess,0.975),
                     rel_excess_med=median(rel_excess),
                     rel_excess_lob=quantile(rel_excess,0.025),
                     rel_excess_upb=quantile(rel_excess,0.975),
                     prob_excess=mean(excess>0),
                     metrics_prob_above=mean(excess>labo_deaths),
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


