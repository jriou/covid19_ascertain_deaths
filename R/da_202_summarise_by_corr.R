#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: summarise samples
#:::::::::::::::::::::::::::::

da_202_summarise_by_corr <- function(dat,by) {
  by = append(by,"it")
  summ = dat %>% 
    dplyr::group_by_at(vars(one_of(by))) %>% 
    dplyr::summarise(deaths=sum(deaths),
                     corr_exp_deaths=sum(corr_exp_deaths),
                     corr_excess=sum(corr_excess),
                     corr_rel_excess=if_else(corr_exp_deaths>0,corr_excess/corr_exp_deaths,0),
                     labo_deaths=sum(labo_deaths),
                     .groups="drop_last") %>%
    dplyr::summarise(corr_exp_deaths_med=median(corr_exp_deaths),
                     corr_exp_deaths_lob=quantile(corr_exp_deaths,0.025),
                     corr_exp_deaths_upb=quantile(corr_exp_deaths,0.975),
                     corr_excess_med=median(corr_excess),
                     corr_excess_lob=quantile(corr_excess,0.025),
                     corr_excess_upb=quantile(corr_excess,0.975),
                     corr_rel_excess_med=median(corr_rel_excess),
                     corr_rel_excess_lob=quantile(corr_rel_excess,0.025),
                     corr_rel_excess_upb=quantile(corr_rel_excess,0.975),
                     metrics_prob_above=mean(corr_excess>labo_deaths),
                     labo_deaths=unique(labo_deaths),
                     deaths=unique(deaths),
                     .groups="drop") %>% 
    dplyr::mutate(metrics_current_corr_excess=if_else(corr_excess_lob>0,1,0),
                  metrics_coverage=if_else(labo_deaths<=corr_excess_upb & labo_deaths>=corr_excess_lob,1,0),
                  metrics_diff_med=corr_excess_med-labo_deaths,
                  metrics_diff_lob=corr_excess_lob-labo_deaths,
                  metrics_diff_upb=corr_excess_upb-labo_deaths,
                  metrics_ascert_med=if_else(metrics_current_corr_excess==1,labo_deaths/corr_excess_med,NA_real_),
                  metrics_ascert_lob=if_else(metrics_current_corr_excess==1,labo_deaths/corr_excess_lob,NA_real_),
                  metrics_ascert_upb=if_else(metrics_current_corr_excess==1,labo_deaths/corr_excess_upb,NA_real_))
  
  return(summ)
}


