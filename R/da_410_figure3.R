#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: plot posterior samples from regression and BMA
#:::::::::::::::::::::::::::::

da_410_figure3 <- function(x,dat) {
  # aesthetics
  col1="firebrick"
  col2="cornflowerblue"
  
  # format data
  x_form = x %>% 
    dplyr::filter(type!="canton") %>% 
    dplyr::mutate(type2=factor(type,
                               levels=c("overall","agegroup","phase"),
                               labels=c("","By age group","By epidemic phase"))) 
  
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
    coord_cartesian(ylim=c(0,4)) +
    theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
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
    coord_cartesian(ylim=c(.75,1.1)) +
    theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
    labs(x=NULL,y=expression(beta[2]))+
    theme(strip.text=element_text(size=7))
  
  # correlation by age

  lims = summ_week_age_temp %>% 
    dplyr::group_by(age_group) %>% 
    dplyr::summarize(lim=max(excess_med,-excess_med))
  slopes = x %>% 
    dplyr::filter(n_beta=="beta_1",type=="agegroup",group!="Hyperparameters") %>% 
    dplyr::rename(age_group=group) %>% 
    dplyr::mutate(beta_1_est = paste0("=",qsum2(beta_med,beta_lb,beta_ub))) %>% 
    dplyr::left_join(lims,by = "age_group")
  
  gA = summ_week_age_temp %>% 
    ggplot() +
    geom_hline(yintercept=0,linetype=1,colour="grey") +
    # geom_pointrange(aes(x=labo_deaths,y=excess_med,ymin=excess_lob,ymax=excess_upb),alpha=.7,size=.3) +
    geom_point(data=lims,aes(x=lim,y=lim),colour="transparent") +
    geom_point(data=lims,x=0,aes(y=-lim),colour="transparent") +
    geom_abline(intercept=0,slope=1,colour="black") +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_med),size=1,colour=col_labd,alpha=.7) +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_lb),colour=col_labd,linetype=5,alpha=.7) +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_ub),colour=col_labd,linetype=5,alpha=.7) +
    geom_point(aes(x=labo_deaths,y=excess_med),alpha=.7,shape=1) +
    # geom_text(data=slopes,aes(y=-0.7*lim,x=0.8*lim,label=beta_1_est),colour=col_labd,size=3) +
    # geom_text(data=slopes,aes(y=-1.02*lim,x=0.78*lim),label=expression(beta[1]),colour=col_labd,size=3) +
    # scale_x_continuous(trans="pseudo_log") +
    facet_wrap(~age_group,scales="free",ncol=1) +
    labs(x="Laboratory-confirmed COVID-19-related deaths",y="Absolute excess mortality") +
    coord_flip()
  gA
  summ_week_age_temp %>% 
    dplyr::left_join(slopes,by = "age_group") %>% 
    dplyr::mutate(residuals=excess_med-beta_med*labo_deaths) %>% 
    dplyr::group_by(age_group) %>% 
    dplyr::mutate(mean_res=mean(residuals)) %>% 
    ggplot() +
    geom_hline(yintercept=0,linetype=1,colour="grey") +
    # geom_pointrange(aes(x=labo_deaths,y=excess_med,ymin=excess_lob,ymax=excess_upb),alpha=.7,size=.3) +
    geom_point(data=lims,aes(x=lim,y=lim),colour="transparent") +
    geom_point(data=lims,x=0,aes(y=-lim),colour="transparent") +
    # geom_segment(aes(x=labo_deaths,xend=labo_deaths,y=excess_med,yend=residuals),colour="skyblue",linetype=3) +
    # geom_point(aes(x=labo_deaths,y=excess_med),alpha=.7,shape=4) +
    geom_point(aes(x=labo_deaths,y=residuals),alpha=.7,shape=4) +
    geom_hline(aes(yintercept=mean_res),colour="skyblue") +
    # scale_x_continuous(trans="pseudo_log") +
    facet_wrap(~age_group,scales="free",ncol=1) +
    labs(x="Laboratory-confirmed COVID-19-related deaths",y="Residual absolute excess mortality") +
    coord_flip()
  
  summ_week_age_temp %>% 
    dplyr::left_join(slopes,by = "age_group") %>% 
    dplyr::mutate(residuals=(excess_med-beta_med*labo_deaths)/exp_deaths_med) %>% 
    dplyr::group_by(age_group) %>% 
    dplyr::mutate(mean_res=mean(residuals)) %>% 
    ggplot() +
    geom_vline(xintercept=0,linetype=1,colour="grey") +
    geom_histogram(aes(residuals),alpha=.7) +
    geom_vline(aes(xintercept=mean_res),colour="skyblue") +
    facet_wrap(~age_group,,ncol=1) +
    labs(x="Residual absolute excess mortality") 
  
  

  # correlation by age
  tt = summ_week_canton_temp %>%
    dplyr::left_join(phases,by = c("phase", "week"))
  lims = tt %>%
    dplyr::group_by(phase) %>%
    dplyr::summarize(lim=max(excess_med,-excess_med))
  slopes = x %>%
    dplyr::filter(n_beta=="beta_1",type=="phase",group!="Hyperparameters") %>%
    dplyr::rename(phase=group)

  gAb = tt %>%
    ggplot() +
    geom_hline(yintercept=0,linetype=1,colour="grey") +
    # geom_pointrange(aes(x=labo_deaths,y=excess_med,ymin=excess_lob,ymax=excess_upb),alpha=.7,size=.3) +
    geom_point(data=lims,aes(x=lim,y=lim),colour="transparent") +
    geom_point(data=lims,x=0,aes(y=-lim),colour="transparent") +
    geom_abline(intercept=0,slope=1,colour="black") +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_med),size=1,colour=col_labd,alpha=.7) +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_lb),colour=col_labd,linetype=5,alpha=.7) +
    geom_abline(data=slopes,aes(intercept=0,slope=beta_ub),colour=col_labd,linetype=5,alpha=.7) +
    geom_point(aes(x=labo_deaths,y=excess_med),alpha=.7,shape=4) +
    # scale_x_continuous(trans="pseudo_log") +
    facet_wrap(~phase,scales="free",ncol=2) +
    labs(x="Laboratory-confirmed COVID-19-related deaths",y="Absolute excess mortality") +
    coord_flip()
  ggsave(gAb,file="figures/suppfig_correlation_by_phase.pdf",width=6,height=7)
  
  
  # format data
  x_form = x %>% 
    dplyr::filter(type=="canton") %>% 
    dplyr::mutate(type2=factor(type,
                               levels=c("canton"),
                               labels=c("By canton"))) 
  
  # coefficient for covid-19 deaths
  ub_lim = 3
  lab_ = x_form %>% 
    dplyr::filter(n_beta=="beta_1",
                  par=="beta")
  g1canton = ggplot(lab_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=lab_overall,colour=col1,linetype=3) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),colour=col1,size=.3) +
    scale_y_continuous() +
    coord_cartesian(ylim=c(0,4)) +
    labs(x=NULL,y=expression(beta[1])) +
    theme(strip.text=element_text(size=7))
  
  # coefficient for expected deaths
  exp_ = x_form %>% 
    dplyr::filter(n_beta=="beta_2",
                  par=="beta")
  g2canton = ggplot(exp_) +
    geom_hline(yintercept=1,colour="grey50") +
    geom_hline(yintercept=exp_overall,linetype=3,colour=col2) +
    geom_pointrange(aes(x=group,y=beta_med,ymin=beta_lb,ymax=beta_ub),colour=col2,size=.3) +
    scale_y_continuous(breaks=seq(0,2,by=.1)) +
    coord_cartesian(ylim=c(.75,1.1)) +
    labs(x=NULL,y=expression(beta[2]))+
    theme(strip.text=element_text(size=7))
  
  gcanton = cowplot::plot_grid(g1canton,g2canton,ncol=1,labels=c("A","B"))
  
  ggsave(gcanton,file="figures/suppfig_beta_by_canton.pdf",width=6,height=7)
  

  # merge plots
  
  
  gB = cowplot::plot_grid(g1,g2,ncol=1,labels=c("B","C"))
  g = cowplot::plot_grid(gA,gB,ncol=2,labels=c("A",""),rel_widths = c(1,1))
  
  return(g)
}


