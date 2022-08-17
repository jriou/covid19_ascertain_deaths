
da_408_figure1 = function() {
  
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
    geom_col(aes(x=phase,y=rel_excess_med,fill=age_group)) +
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
  
  # time-space panel
  cols_exp = brewer.pal(n = 11, name = "RdBu")[c(6, 5:1)]
  date_max = max(summ_week_canton_temp$week)
  date_phases2 = date_phases %>%
    dplyr::mutate(end_date=ymd(ifelse(end_date>date_max,as.character(date_max+6),
                                      as.character(end_date)))) 
  
  g_ts = summ_week_canton_temp %>% 
    dplyr::mutate(cut_rel_excess_med=base::cut(rel_excess_med*100,
                                               breaks=c(-Inf,0,10,50,100,Inf),
                                               labels=c("<0%","0-10%","10-50%","50-100%",">100%"))) %>% 
    ggplot() +
    geom_raster(aes(x=week,y=canton,fill=cut_rel_excess_med)) +
    scale_fill_manual(values=cols_exp,na.value = "white") +
    geom_label(data=date_phases2,aes(x=start_date+(end_date-start_date)/2,y=27.4,label=phase),
               size=2.5) +
    geom_vline(data=date_phases2[-1,],aes(xintercept=start_date)) +
    scale_y_discrete(expand=expansion(0,0),
                     limits=rev) +
    coord_cartesian(clip="off",ylim=c(0.5,26.5)) +
    scale_x_date(date_labels = "%e %b\n%Y",
                 breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                 minor_breaks = NULL,
                 expand=expansion(0,c(0,2.5))) +
    labs(x=NULL,y="Canton",fill="Relative excess mortality:") +
    theme(legend.position="bottom",
          legend.key = element_rect(colour="black"),
          plot.margin = margin(0.7, 0.7, 0, 0.3, "cm"))
  
  # map panel probability of excess
  shp = read_sf("data/shp.shp") %>% 
    left_join(cantons_ids,by=c("ID_PE"="canton_id"))
  
  shp2plot = summ_phase_canton_temp %>% 
    dplyr::left_join(cantons_ids,by="canton") %>% 
    dplyr::left_join(shp,by="canton_name") %>% 
    dplyr::mutate(cut_prob_excess=cut(prob_excess,
                                      breaks=c(-1,.05,.2,.8,.95,1.5),
                                      labels=c("0-5%","5-20%","20-80%","80-95%","95-100%"))) %>% 
    st_as_sf()
  
  cols_exd = brewer.pal(n = 11, name = "RdBu")
  cols_exd = c(cols_exd[c(2, 4, 6, 8, 10)])
  
  g_map = ggplot(shp2plot) +
    geom_sf(col = "black", size = 0.1, aes(fill = cut_prob_excess)) + 
    scale_fill_manual(values=cols_exd[length(cols_exd):1], drop=FALSE) +
    facet_wrap(vars(phase), ncol = 4) + 
    labs(x=NULL,y=NULL,fill="Probability of excess mortality:") +
    theme_bw() +
    theme(legend.position="bottom",
          axis.text=element_blank(),
          axis.ticks = element_blank())
  
  # map panel relative excess
  shp2plot2 = summ_phase_canton_temp %>% 
    dplyr::left_join(cantons_ids,by="canton") %>% 
    dplyr::left_join(shp,by="canton_name") %>% 
    dplyr::mutate(cut_rel_excess=cut(rel_excess_med,
                                     breaks=c(-0.2,-0.05,0,0.05,0.1,0.2,1))) %>% 
    st_as_sf()
  
  cols_exd = brewer.pal(n = 11, name = "RdBu")
  cols_exd = c(cols_exd[c(2, 4, 6, 8, 10)])
  
  g_map2 = ggplot(shp2plot2) +
    geom_sf(col = "black", size = 0.1, aes(fill = rel_excess_med)) + 
    scale_fill_gradient2(low="white",high=col_excess1,midpoint=0,labels=scales::percent,trans="pseudo_log") +
    facet_wrap(vars(phase), ncol = 4) + 
    labs(x=NULL,y=NULL,fill="Relative excess\nmortality:") +
    theme_bw() +
    theme(legend.position=c(.9,.2),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_text(size=9,vjust=1),
          legend.text = element_text(size=8),
          legend.key.height = unit(.5,"cm"),
          legend.key.width = unit(.5,"cm"),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  
  # join
  # plot_grid(g_ts,g_map,ncol=1,labels=LETTERS,rel_heights = c(1.3,1))
  plot_grid(grel2b,g_map2,ncol=1,labels=LETTERS,rel_heights = c(1,1.5))
}