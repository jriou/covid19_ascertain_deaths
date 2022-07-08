#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format table
#:::::::::::::::::::::::::::::

da_406_format_table <- function(summaries,betas,indirect) {
  qsumx = function(a,b,c) paste0(formatC(a, format="f", big.mark=",", digits=0),
                                " (95% CrI: ",
                                formatC(b, format="f", big.mark=",", digits=0),
                                " to ",
                                formatC(c, format="f", big.mark=",", digits=0),
                                ")")
  qsumpercx = function(a,b,c) paste0(formatC(a*100, format="f", big.mark=",", digits=0),
                                    "% (95% CrI: ",
                                    formatC(b*100, format="f", big.mark=",", digits=0),
                                    " to ",
                                    formatC(c*100, format="f", big.mark=",", digits=0),
                                    ")")
  tab = summaries %>% 
    dplyr::transmute(laboratory=formatC(labo_deaths, format="f", big.mark=",", digits=0),
                  observed_deaths=formatC(deaths, format="f", big.mark=",", digits=0),
                  exp_deaths=qsumx(exp_deaths_med,exp_deaths_lob,exp_deaths_upb),
                  exp_deaths=qsumx(exp_deaths_med,exp_deaths_lob,exp_deaths_upb),
                  excess=qsumx(excess_med,excess_lob,excess_upb)) %>% 
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
    dplyr::mutate(direct_caused=qsumx(beta_med*labo_deaths,beta_lb*labo_deaths,beta_ub*labo_deaths),
                  ascertainment=qsumpercx(1/beta_med,1/beta_ub,1/beta_lb)) %>% 
    dplyr::select(direct_caused,ascertainment) %>% 
    tidyr::pivot_longer(1:2) %>% 
    dplyr::bind_rows(tab,.)
  
  tab = indirect %>% 
    dplyr::mutate(indirect_deaths=qsumx(indirect_deaths_med,indirect_deaths_lob,indirect_deaths_upb)) %>% 
    dplyr::select(indirect_deaths) %>% 
    tidyr::pivot_longer(1) %>% 
    dplyr::bind_rows(tab,.)
  
  tab$name = c("Laboratory-confirmed COVID-19 deaths",
              "Observed all-cause deaths",
              "Expected all-cause deaths",
              "Excess all-cause deaths",
              "Deaths directly attributable to COVID-19 for each laboratory-confirmed death ($\\beta_1$)$^a$",
              "Observed all-cause deaths for each expected all-cause deaths ($\\beta_2$)$^b$",
              "Deaths directly attributable to COVID-19",
              "Ascertainment of deaths directly attributable to COVID-19",
              "Deaths indirectly caused or averted during the pandemic")
  
  return(tab)
}


