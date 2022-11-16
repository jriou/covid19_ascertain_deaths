#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format table
#:::::::::::::::::::::::::::::

da_412_format_table_by_phase_corr <- function(summaries) {
  date_max = max(summ_week_temp$week)
  date_phases2 = date_phases %>%
    dplyr::mutate(end_date=ymd(ifelse(end_date>date_max,as.character(date_max+6),as.character(end_date))))
  tab = summaries %>% 
    dplyr::transmute(
      phase=as.character(phase),
      laboratory=formatC(labo_deaths, format="f", big.mark=",", digits=0),
      observed_deaths=formatC(deaths, format="f", big.mark=",", digits=0),
      corr_exp_deaths=qsum(corr_exp_deaths_med,corr_exp_deaths_lob,corr_exp_deaths_upb),
      corr_exp_deaths=qsum(corr_exp_deaths_med,corr_exp_deaths_lob,corr_exp_deaths_upb),
      corr_excess=qsum(corr_excess_med,corr_excess_lob,corr_excess_upb),
      corr_rel_excess=qsumperc(corr_rel_excess_med,corr_rel_excess_lob,corr_rel_excess_upb)) %>% 
    left_join(date_phases2,by = "phase") %>% 
    mutate(dates=paste0(format(start_date,"%b %-d, %Y")," to ",format(end_date,"%b %-d, %Y"))) %>% 
    dplyr::select(phase,corr_exp_deaths,observed_deaths,corr_excess,corr_rel_excess,laboratory)
  
  
  names(tab) = c(
    "Phase*",
    "Expected",
    "Observed",
    "Excess",
    "Relative corr_excess",
    "Laboratory-confirmed COVID-19 deaths")
  
  return(tab)
}


