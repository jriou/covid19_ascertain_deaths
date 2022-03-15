#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: plot posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_402_plot_regbma <- function(x,panel_labels=NULL) {
  # esthetics
  col1="firebrick"
  col2="cornflowerblue"
  
  # format data
  x_form = x %>% 
    dplyr::mutate(type2=factor(type,
                               levels=c("tot","phase","agegroup","canton"),
                               labels=c("","By epidemic phase","By age group","By canton")))
  
  # coefficient for covid-19 deaths
  ub_lim = 5
  lab_ = x_form %>% 
    dplyr::filter(n_beta=="beta_1")
  lab_overall = filter(lab_,group=="Overall") %>% pull(beta_med)
  g1 = ggplot(lab_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=lab_overall,linetype=3,colour=col1) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),
                    colour=col1) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous() +
    coord_cartesian(ylim=c(0,6)) +
    theme(axis.text.x=element_text(angle=90,vjust=.5)) +
    labs(x=NULL,y="beta_1")
  
  # coefficient for expected deaths
  exp_ = x_form %>% 
    dplyr::filter(n_beta=="beta_2")
  exp_overall = filter(exp_,group=="Overall") %>% pull(beta_med)
  g2 = ggplot(exp_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=exp_overall,linetype=3,colour=col2) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),
                    colour=col2) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous(breaks=seq(0,2,by=.1)) +
    coord_cartesian(ylim=c(.6,1.1)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
    labs(x=NULL,y="beta_2")
  
  # merge plots
  return(cowplot::plot_grid(g1,g2,ncol=1,labels=panel_labels))
}


