#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format table
#:::::::::::::::::::::::::::::

da_408_format_table_by_phase <- function(summaries) {
  date_max = max(summ_week_temp$week)
  date_phases2 = date_phases %>%
    dplyr::mutate(end_date=ymd(ifelse(end_date>date_max,as.character(date_max+6),as.character(end_date))))
  tab = summaries %>% 
    dplyr::transmute(
      phase=as.character(phase),
      laboratory=formatC(labo_deaths, format="f", big.mark=",", digits=0),
      observed_deaths=formatC(deaths, format="f", big.mark=",", digits=0),
      exp_deaths=qsum(exp_deaths_med,exp_deaths_lob,exp_deaths_upb),
      exp_deaths=qsum(exp_deaths_med,exp_deaths_lob,exp_deaths_upb),
      excess=qsum(excess_med,excess_lob,excess_upb),
      rel_excess=qsumperc(rel_excess_med,rel_excess_lob,rel_excess_upb)) %>% 
    left_join(date_phases2,by = "phase") %>% 
    mutate(dates=paste0(format(start_date,"%b %-d, %Y")," to ",format(end_date,"%b %-d, %Y"))) %>% 
    dplyr::select(phase,dates,exp_deaths,observed_deaths,excess,rel_excess,laboratory)
  
  
  names(tab) = c(
    "Epidemic phase",
    "Dates",
    "Expected all-cause deaths (95% credible interval)",
    "Observed all-cause deaths",
    "Excess all-cause deaths (95% credible interval)",
    "Relative excess all-cause deaths (95% credible interval)",
    "Laboratory-confirmed COVID-19 deaths")
  
  return(tab)
}


