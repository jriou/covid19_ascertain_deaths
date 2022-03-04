#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: plot posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_402_plot_regbma <- function(x) {
  # esthetics
  col1="cornflowerblue"
  col2="firebrick"
  
  # format data
  x_form = x %>% 
    dplyr::mutate(type2=factor(type,
                               levels=c("tot","phase","agegroup","canton"),
                               labels=c("","By epidemic phase","By age group","By canton")))
  
  # coefficient for expected deaths
  exp_ = x_form %>% 
    dplyr::filter(n_beta=="A")
  exp_overall = filter(exp_,group=="Overall") %>% pull(beta_med)
  g1 = ggplot(exp_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=exp_overall,linetype=3,colour=col1) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),
                    colour=col1) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous(breaks=seq(0,2,by=.1),limits=c(.6,1.1)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
    labs(x=NULL,y="beta_2")
  print(g1)
  g1b = ggplot(exp_) +
    geom_hline(yintercept=0,colour="grey50") +
    geom_hline(yintercept=exp_overall-1,linetype=3,colour=col1) +
    geom_pointrange(aes(x=group,y=beta_med-1,ymin=beta_lb-1,ymax=beta_ub-1),
                    colour=col1) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous(breaks=seq(-1,2,by=.1)) +
    coord_cartesian(ylim=c(-.5,0.1)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) +
    labs(x=NULL,y="beta_2-1")
  print(g1b)
  # coefficient for covid-19 deaths
  ub_lim = 5
  lab_ = x_form %>% 
    dplyr::filter(n_beta=="B") %>%
    dplyr::mutate(asc_med=beta_med-1,
                  asc_lb=beta_lb-1,
                  asc_ub=beta_ub-1) #%>% 
    # dplyr::mutate(asc_med=if_else(asc_lb<0,NA_real_,asc_med))
  lab_overall = filter(lab_,group=="Overall") %>% pull(beta_med)
  g2 = ggplot(lab_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=lab_overall,linetype=3,colour=col2) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),
                    colour=col2) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous() +
    coord_cartesian(ylim=c(0,6)) +
    theme(axis.text.x=element_text(angle=90,vjust=.5)) +
    labs(x=NULL,y="beta_1")
  print(g2)
  g2b = lab_ %>% 
    ggplot() +
    geom_hline(yintercept=0,colour="grey50") +
    geom_hline(yintercept=lab_overall-1,linetype=3,colour=col2) +
    geom_pointrange(aes(x=group,y=asc_med,ymin=asc_lb,ymax=asc_ub),
                    colour=col2) +
    facet_grid(. ~ type2,scales="free",space = "free") +
    scale_y_continuous(breaks=-1:6) +
    coord_cartesian(ylim=c(-1,5)) +
    theme(axis.text.x=element_text(angle=90,vjust=.5)) +
    labs(x=NULL,y="beta_1 - 1")
  print(g2b)
}


