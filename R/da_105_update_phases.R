if(FALSE) {
  # update phase 7 (add if needed)
  date_update = tibble(phase="7",
                       start_date=as.Date("2021-12-20"),
                       end_date=end_date)
  
  date_phases = bind_rows(date_phases,date_update)
  phases = bind_rows(phases,
                     tibble(phase=7,week=seq.Date(from=date_update$start_date,to=date_update$end_date,by=7)))
  
  saveRDS(phases,file="data/phases.rds")
  
  
}
