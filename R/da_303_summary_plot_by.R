#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_303_summary_plot_by <- function(dat) {
  # filter
  date_max = max(dat$week)
  date_phases2 = date_phases %>%
    dplyr::mutate(end_date=ymd(ifelse(end_date>date_max,as.character(date_max+6),as.character(end_date))))
  jan1= tibble(week=as.Date(c("2020-01-01","2021-01-01","2022-01-01")))
  date_phases3 = dat %>% 
    dplyr::group_by(age_group,phase) %>% 
    dplyr::summarise(start_date=min(week),
                     end_date=max(week),
                     lim=max(excess_upb,labo_deaths),
                     .groups="drop_last") %>% 
    dplyr::mutate(lim=max(lim))
  # sort out limits
  lims = c(min(dat$excess_upb)*1.1,max(dat$excess_upb)*1.15)
  # plot weekly counts
  g = dat %>% 
    ggplot(aes(x=week)) +
    # geom_vline(data=jan1,aes(xintercept=week), colour="grey50",alpha=.2,linetype=2) +
    geom_hline(yintercept=0,col="grey50") +
    geom_ribbon(aes(ymin=excess_lob,ymax=excess_upb,fill=col_excess1),alpha=.3) +
    geom_line(aes(y=labo_deaths,col="col")) +
    geom_point(aes(y=labo_deaths,col="col"),shape=21,fill="white") +
    geom_label(data=date_phases3,aes(x=start_date+(end_date-start_date)/2,y=lim*1.15,label=phase),size=2.5,colour="black") +
    geom_point(data=date_phases3,aes(x=start_date+(end_date-start_date)/2,y=-lim*1.15),colour="transparent") +
    labs(x=NULL,y="Weekly count") +
    scale_x_date(date_labels = "%e %b %Y",
                 breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                 minor_breaks = NULL,
                 expand=expansion(add=c(4,10))) +
    scale_y_continuous(expand=expansion(mult=c(0.1,0.08))) + 
    scale_fill_identity(name = NULL, guide = 'legend', labels = c('Absolute excess mortality')) +
    scale_colour_manual(name = NULL, values =c("col"=col_labd), labels = "Laboratory-confirmed COVID-19-related deaths") +
    facet_wrap(~age_group,scales="free",ncol=2)+
    theme(plot.margin = unit(c(3,3,3,3), "mm"),
          legend.position = c(.75,.15),
          legend.spacing = unit(2,"mm"),
          legend.text=element_text(size=7.5),
          legend.key.height = unit(0,"mm"),
          legend.background = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1),
          axis.title=element_text(size=9))
  g
  return(g)
}


