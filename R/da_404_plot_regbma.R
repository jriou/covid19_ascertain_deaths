#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: plot posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_404_plot_regbma <- function(x,panel_labels=NULL) {
  # esthetics
  col1="firebrick"
  col2="cornflowerblue"
  
  # reformat cantons
  x = x %>% 
    dplyr::mutate(group=dplyr::case_when(group=="Appenzell Ausserrhoden" ~ "Appenzell Ausser.",
                                         group=="Appenzell Innerrhoden" ~ "Appenzell Inner.",
                                         group=="Bern / Berne" ~ "Bern",
                                         group=="Fribourg / Freiburg" ~ "Fribourg",
                                         group=="Graubünden / Grigioni / Grischun" ~ "Graubünden",
                                         group=="Valais / Wallis" ~ "Valais",
                                         TRUE ~ group))
  
  # format data
  x_form = x %>% 
    dplyr::mutate(type2=factor(type,
                               levels=c("overall","phase","agegroup","canton"),
                               labels=c("","By epidemic phase","By age group","By canton")))
  
  # coefficient for covid-19 deaths
  ub_lim = 5
  lab_ = x_form %>% 
    dplyr::filter(n_beta=="beta_1",
                  par=="beta")
  lab_overall = filter(lab_,group=="Overall") %>% pull(beta_med)
  lab_means = x_form %>% 
    dplyr::filter(n_beta=="beta_1",
                  group %in% c("Hyperparameters"),
                  par!="sd.beta")
  g1 = ggplot(lab_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=lab_overall,colour=col1,linetype=3) +
    # geom_hline(data=lab_means,aes(yintercept=beta_med),colour=col1,size=1,alpha=.3) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),colour=col1,size=.3) +
    # geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub,colour=prob1)) +
    # scale_colour_gradient2(low="red",mid="orange",high="skyblue",midpoint = 0.1,trans="pseudo_log") +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous() +
    coord_cartesian(ylim=c(0,3.5)) +
    theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1)) +
    labs(x=NULL,y=expression(beta[1])) +
    theme(strip.text=element_text(size=7))
  
  # coefficient for expected deaths
  exp_ = x_form %>% 
    dplyr::filter(n_beta=="beta_2",
                  par=="beta")
  exp_overall = filter(exp_,group=="Overall") %>% pull(beta_med)
  exp_means = x_form %>% 
    dplyr::filter(n_beta=="beta_2",
                  group %in% c("Hyperparameters"),
                  par!="sd.beta")
  g2 = ggplot(exp_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=exp_overall,linetype=3,colour=col2) +
    # geom_hline(data=exp_means,aes(yintercept=beta_med),colour=col2,size=1,alpha=.3) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),colour=col2,size=.3) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous(breaks=seq(0,2,by=.1)) +
    coord_cartesian(ylim=c(.75,1.05)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) +
    labs(x=NULL,y=expression(beta[2]))+
    theme(strip.text=element_text(size=7))

  # merge plots
  return(cowplot::plot_grid(g1,g2,ncol=1,labels=panel_labels))
}


