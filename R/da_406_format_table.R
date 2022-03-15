#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format table
#:::::::::::::::::::::::::::::

da_406_format_table <- function(summaries,betas) {
  tab = summaries %>% 
    dplyr::transmute(laboratory=formatC(labo_deaths, format="f", big.mark=",", digits=0),
                  observed_deaths=formatC(deaths, format="f", big.mark=",", digits=0),
                  exp_deaths=qsum(exp_deaths_med,exp_deaths_lob,exp_deaths_upb),
                  excess=qsum(excess_med,excess_lob,excess_upb)) %>% 
    tidyr::pivot_longer(cols=1:4)
  
  tab = betas %>% 
    dplyr::filter(group=="Overall") %>% 
    dplyr::mutate(value=qsum2(beta_med,beta_lb,beta_ub)) %>% 
    dplyr::select(name=n_beta,value) %>% 
    dplyr::arrange(name) %>% 
    dplyr::bind_rows(tab,.)
  
  tab = betas %>% 
    dplyr::filter(group=="Overall",n_beta=="beta_1") %>% 
    dplyr::mutate(labo_deaths=summaries$labo_deaths) %>% 
    dplyr::mutate(direct_caused=qsum(beta_med*labo_deaths,beta_lb*labo_deaths,beta_ub*labo_deaths),
                  ascertainment=qsumperc(1/beta_med,1/beta_ub,1/beta_lb)) %>% 
    dplyr::select(direct_caused,ascertainment) %>% 
    tidyr::pivot_longer(1:2) %>% 
    dplyr::bind_rows(tab,.)
  
  tab$name = c("Laboratory-confirmed COVID-19 deaths",
              "Observed all-cause deaths",
              "Expected all-cause deaths",
              "Excess all-cause deaths",
              "Deaths directly attributable to SARS-CoV-2 for each laboratory-confirmed death ($\\beta_1$)$^a$",
              "Observed all-cause deaths for each expected all-cause deaths ($\\beta_2$)$^b$",
              "Deaths directly attributable to SARS-CoV-2",
              "Ascertainment of deaths directly attributable to SARS-CoV-2")
  
  return(tab)
}


