#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format beta estimates
#:::::::::::::::::::::::::::::

da_405_format_beta <- function(x,select_group=NULL,select_beta=NULL,trans="",digits=2) {
  unit=""
  if(!is.null(select_group)) x = x %>% dplyr::filter(group==select_group)
  if(!is.null(select_beta)) x = x %>% dplyr::filter(n_beta==select_beta)
  if(trans=="-1") {
    x = x %>% dplyr::mutate(beta_med=(beta_med-1)*100,
                                    beta_lb=(beta_lb-1)*100,
                                    beta_ub=(beta_ub-1)*100)
    unit = "%"
  }
  if(trans=="1-") {
    x = x %>% dplyr::mutate(beta_med=(1-beta_med)*100,
                            x = beta_lb,
                            beta_lb=(1-beta_ub)*100,
                            beta_ub=(1-x)*100) %>% 
      dplyr::select(-x)
    unit = "%"
  }
  if(trans=="1/") {
    x = x %>% dplyr::mutate(beta_med=1/beta_med*100,
                            tmp=beta_lb,
                            beta_lb=1/beta_ub*100,
                            beta_ub=1/tmp*100) 
    unit = "%"
  }
  res = x %>% 
    dplyr::mutate(y=paste0(formatC(beta_med,digits=digits,format="f"),
                              unit," (95%CrI: ",
                              formatC(beta_lb,digits=digits,format="f"),
                              " to ",
                              formatC(beta_ub,digits=digits,format="f"),
                              ")")) %>%
    pull(y)
  return(res)
}


