#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: summarise samples
#:::::::::::::::::::::::::::::

da_204_summarise_indirect <- function(dat,betas) {
  by = "it"
  summ = dat %>% 
    dplyr::group_by_at(vars(one_of(by))) %>% 
    dplyr::summarise(exp_deaths=sum(exp_deaths),
                     .groups="drop_last") %>%
    dplyr::mutate(beta_2=betas$Total$`beta[1]:exp`) %>% 
    dplyr::mutate(indirect_deaths=-exp_deaths*(1-beta_2)) %>% 
    dplyr::summarise(indirect_deaths_med=median(indirect_deaths),
                     indirect_deaths_lob=quantile(indirect_deaths,0.025),
                     indirect_deaths_upb=quantile(indirect_deaths,0.975),
                     .groups="drop") 
  return(summ)
}


