
da_409_figure1 = function() {
  
  # expected and observed
  date_max = max(summ_week_age_temp$week)
  date_phases2 = date_phases %>%
    dplyr::mutate(end_date=ymd(ifelse(end_date>date_max,as.character(date_max+6),as.character(end_date))))
  date_phases3 = summ_week_age_temp %>% 
    dplyr::group_by(age_group,phase) %>% 
    dplyr::summarise(start_date=min(week),
              end_date=max(week),
              lim=max(exp_deaths_upb,deaths),
              .groups="drop_last") %>% 
    dplyr::mutate(lim=max(lim))
  
  leg = c("Expected deaths"="grey",
          "Observed deaths"="skyblue",
          "Laboratory-confirmed deaths"=col_labd)
  
  gabs = summ_week_age_temp %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(aes(ymin=exp_deaths_lob,ymax=exp_deaths_upb,fill="grey"),alpha=.4) +
    geom_line(aes(y=exp_deaths_med),colour="grey") +
    geom_line(aes(y=deaths,colour="col")) +
    # geom_col(aes(y=labo_deaths,fill=col_labd)) +
    annotate("point",x=min(summ_week_age_temp$week),y=0,colour="transparent") +
    facet_wrap(~age_group,scales="free",ncol=5) +
    geom_label(data=date_phases3,aes(x=start_date+(end_date-start_date)/2,y=lim*1.15,label=phase),size=1.8,colour="black") +
    labs(x=NULL,y="Weekly deaths") +
    scale_x_date(date_labels = "%b %e %Y",
                 breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                 minor_breaks = NULL,
                 expand=expansion(add=c(4,10))) +
    scale_y_continuous(expand=expansion(mult=c(0,.07))) +
    scale_fill_identity(name=NULL,guide='legend',labels=c("Expected deaths")) +
    scale_colour_manual(name = NULL, values =c("col"=col_expected), labels = "Observed deaths") +
    theme(legend.position = c(.5,.15),
          legend.spacing = unit(0,"mm"),
          legend.text=element_text(size=7.5),
          legend.key.height = unit(0,"mm"),
          legend.background = element_blank(),
          legend.margin = margin(0,0,0,0),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1),
          axis.title=element_text(size=9))
  
  
  
  
  # relative excess by x
  grel1 = dplyr::bind_rows(
    dplyr::mutate(summ_all_temp,type="",lab="Overall"),
    dplyr::mutate(summ_phase_temp,type="By epidemic phase",lab=as.character(phase)),
    dplyr::mutate(summ_age_temp,type="By age group",lab=age_group),
    dplyr::mutate(summ_canton_temp,type="By canton",lab=canton)
  ) %>% 
    dplyr::mutate(type=factor(type,levels=c("","By epidemic phase","By age group","By canton"))) %>% 
    ggplot() +
    geom_hline(yintercept=0,colour="grey50") +
    geom_col(aes(x=lab,y=rel_excess_med),fill=col_excess1) +
    geom_errorbar(aes(x=lab,y=rel_excess_med,ymin=rel_excess_lob,ymax=rel_excess_upb),
                  colour="black",width=.3,size=.3,alpha=.8) +
    facet_grid(. ~ type,scales="free",space = "free") +
    theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1)) +
    scale_y_continuous(labels=scales::percent) +
    labs(x=NULL,y="Relative excess all-cause deaths") +
    theme(strip.text=element_text(size=7))
  
  # relative excess by age and phase
  grel2a = summ_age_phase_temp %>% 
    dplyr::mutate(phase=as.character(phase)) %>% 
    ggplot() +
    geom_hline(yintercept=0,colour="grey50") +
    geom_col(aes(x=phase,y=rel_excess_med),fill=col_excess1,colour="black",size=.3,width=.8,alpha=.5) +
    geom_errorbar(aes(x=phase,y=rel_excess_med,ymin=rel_excess_lob,ymax=rel_excess_upb),
                  colour="black",width=.3,size=.3,alpha=.8) +
    facet_grid(. ~ age_group,scales="free",space = "free") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_discrete(guide="none") +
    labs(x="Epidemic phase",y="Relative excess mortality") 
  
  grel2b = summ_age_phase_temp %>% 
    dplyr::mutate(phase=as.character(phase)) %>% 
    ggplot() +
    geom_hline(yintercept=0,colour="grey50") +
    geom_col(aes(x=age_group,y=rel_excess_med),fill=col_excess1,colour="black",size=.3,width=.8,alpha=.5) +
    geom_errorbar(aes(x=age_group,y=rel_excess_med,ymin=rel_excess_lob,ymax=rel_excess_upb),
                  colour="black",width=.3,size=.3) +
    facet_grid(. ~ phase,scales="free",space = "free") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_discrete(guide="none") +
    labs(x="Age group",y="Relative excess mortality") +
    theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1),
          axis.title=element_text(size=9))
  
  # stringency
  reverse2_trans <- function() {
    scales::trans_new(
      "reverse2",
      function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
      function(x) -1 * as.numeric(x)
    )
  }
  suppressWarnings(stringency <- read_csv("data/owid-covid-data.csv",col_types = cols()) )
  gstring = stringency %>%
    dplyr::filter(location=="Switzerland",date<=date_max) %>% 
    dplyr::select(location,date,stringency_index) %>%
    ggplot() +
    geom_step(aes(x=date,y=stringency_index),colour="orange") +
    geom_label(data=date_phases2,aes(x=start_date+(end_date-start_date)/2,y=85,label=phase),size=1.8,colour="black") +
    labs(x=NULL,y="Oxford stringency index") +
    # scale_x_date(date_labels = "%b %e %Y",
    #              breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
    #              minor_breaks = NULL,
    #              expand=expansion(add=c(4,10)),
    #              trans = trans = c("date","reverse2")) +
    scale_x_continuous(trans = c("date","reverse2"),
                       breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                       labels=format(c(date_phases2$start_date,max(date_phases2$end_date)),"%b %e %Y")) +
    coord_flip(ylim=c(10,90))
  gstring2 = stringency %>%
    dplyr::filter(location=="Switzerland",date<=date_max) %>% 
    dplyr::select(location,date,stringency_index) %>%
    ggplot() +
    geom_step(aes(x=date,y=stringency_index),colour="orange") +
    geom_label(data=date_phases2,aes(x=start_date+(end_date-start_date)/2,y=85,label=phase),size=1.8,colour="black") +
    labs(x=NULL,y="Oxford stringency index") +
    scale_x_date(date_labels = "%b %e %Y",
                 breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                 minor_breaks = NULL,
                 expand=expansion(add=c(4,10))) +
    theme(legend.position = c(.5,.15),
          legend.spacing = unit(0,"mm"),
          legend.text=element_text(size=7.5),
          legend.key.height = unit(0,"mm"),
          legend.background = element_blank(),
          legend.margin = margin(0,0,0,0),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1),
          axis.title=element_text(size=9))
  
  # join
  # plot_grid(g_ts,g_map,ncol=1,labels=LETTERS,rel_heights = c(1.3,1))
  # g1 = cowplot::plot_grid(gabs,grel2a,ncol=1,labels=LETTERS,rel_heights = c(1,1),align="v",axis = "l")
  # plot_grid(g1,gstring,rel_widths = c(3,1),labels=c("","C"))
  plot_grid(
    gabs,
    plot_grid(grel2a,gstring2,rel_widths=c(2.5,1),labels=c("B","C")),
    ncol=1,
    labels=c("A","")
  )
}
