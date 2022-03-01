#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_301_summary_plot <- function(dat) {
  current_excess = dat %>% 
    filter(metrics_current_excess==1) %>% 
    select(week) %>% 
    mutate(wave=case_when(week<as.Date("2020-05-01") ~ "A",
                          week>as.Date("2021-02-01") ~ "C",
                          TRUE ~ "B"))
  lims = c(-max(dat$excess_upb)*0.75,max(dat$excess_upb)*1.5)
  g = dat %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(aes(ymin=excess_lob,ymax=excess_upb,fill=col_excess1),alpha=.3) +
    # geom_line(aes(y=excess_med),col=col_excess1,alpha=.3,size=1.1) +
    geom_line(aes(y=labo_deaths,col="col")) +
    geom_point(aes(y=labo_deaths,col="col"),shape=21,fill="white") +
    # geom_label(data=date_phases,aes(x=start_date,y=950,label=phase),size=2.5) +
    # geom_point(data=current_excess,y=lims[1],colour="orange",shape=17) +
    # annotate("text",y=lims[1]*.9,x=as.Date(c("2020-04-04","2020-11-28","2021-12-07")),
             # label=c("A","B","C"),colour="orange",hjust=.5,size=3) +
    coord_cartesian(ylim=lims) +
    labs(x="Time",y="Weekly count") +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_identity(name = NULL, guide = 'legend', labels = c('Excess deaths')) +
    scale_colour_manual(name = NULL, values =c("col"=col_labd), labels = "Laboratory-confirmed deaths") +
    theme(legend.position = c(.7,.75),
          legend.spacing = unit(0,"mm"),
          legend.text=element_text(size=7.5),
          legend.key.height = unit(0,"mm"),
          legend.background = element_blank(),
          legend.margin = margin(0,0,0,0))
  
  return(g)
}


