#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: format posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_302_summary_plot_corr <- function(dat) {
  # filter
  dat = dat %>% 
    dplyr::filter(week<as.Date("2021-12-20"))
  date_phases2 = date_phases %>% 
    dplyr::filter(start_date<as.Date("2021-12-20"))
  # sort out limits
  lims = c(min(dat$corr_excess_upb)*1.1,max(dat$corr_excess_upb)*1.15)
  # plot weekly counts
  g1 = dat %>% 
    ggplot(aes(x=week)) +
    geom_hline(yintercept=0,col="grey50") +
    geom_ribbon(aes(ymin=corr_excess_lob,ymax=corr_excess_upb,fill=col_excess1),alpha=.3) +
    geom_line(aes(y=labo_deaths,col="col")) +
    geom_point(aes(y=labo_deaths,col="col"),shape=21,fill="white") +
    geom_label(data=date_phases2,aes(x=start_date+(end_date-start_date)/2,y=lims[2],label=phase),size=2.5,colour="grey50") +
    labs(x=NULL,y="Weekly count") +
    scale_x_date(date_labels = "%e %b\n%Y",
                 breaks=c(date_phases2$start_date,max(date_phases2$end_date)),
                 minor_breaks = NULL,
                 expand=expansion(add=c(4,10))) +
    scale_y_continuous(expand=expansion(mult=c(0.1,0.08))) + 
    scale_fill_identity(name = NULL, guide = 'legend', labels = c('corr_excess all-cause deaths')) +
    scale_colour_manual(name = NULL, values =c("col"=col_labd), labels = "Laboratory-confirmed SARS-CoV-2-related deaths") +
    theme(legend.position = c(.75,.75),
          legend.spacing = unit(0,"mm"),
          legend.text=element_text(size=7.5),
          legend.key.height = unit(0,"mm"),
          legend.background = element_blank(),
          legend.margin = margin(0,0,0,0),
          axis.text.x=element_text(angle=90,hjust=.5,vjust=0.5))
  # plot probability of corr_excess>lab
  g2 = ggplot(dat) +
    geom_tile(aes(x=week,y="Probability",fill=metrics_prob_above),height=1) +
    scale_fill_gradientn(colours=  c("grey90","black"),limits=c(0,1),guide="none") +
    theme_void()
  # insert g2 into g1
  g = cowplot::ggdraw() +
    cowplot::draw_plot(g1) +
    cowplot::draw_plot(g2, x=0.053,y=0.21,width=.973, height = .04)
  return(g)
}


