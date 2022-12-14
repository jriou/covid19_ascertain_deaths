# Sensitivity analysis with correction of population

res_corr = da_411_format_table_corr(summ_all_temp_corr,summ_regbma_corr,summ_all_temp_indirect_corr) %>% 
  slice(c(3,4,5,8,6)) %>% 
  rename(sensitivity=value) %>% 
  left_join(res) %>% 
  rename(initial=value) %>% 
  relocate(name,initial,sensitivity)
print(xtable::xtable(res_corr),include.rownames = FALSE)


table1_corr = da_412_format_table_by_phase_corr(summ_phase_temp_corr)
print(xtable::xtable(table1_corr), include.rownames = FALSE)

x = merg %>% 
  dplyr::select(week,age_group,population,shaved_pop,it) %>% 
  dplyr::group_by(week,age_group,it) %>% 
  dplyr::summarise(pop=sum(population),
                   shaved_pop=sum(shaved_pop),
                   .groups="drop_last") %>% 
  dplyr::summarise(pop_mean=mean(pop),
                   pop_lob=quantile(pop,0.05),
                   pop_upb=quantile(pop,0.95),
                   shaved_pop_mean=mean(shaved_pop),
                   shaved_pop_lob=quantile(shaved_pop,0.05),
                   shaved_pop_upb=quantile(shaved_pop,0.95))

suppfigpop = x %>% 
  ggplot(aes(x=week)) +
  geom_ribbon(aes(ymin=pop_lob,ymax=pop_upb),fill="firebrick2",alpha=.5) +
  geom_line(aes(y=pop_mean),colour="firebrick2") +
  geom_line(aes(y=shaved_pop_mean),colour="forestgreen") +
  geom_line(aes(y=shaved_pop_lob),colour="forestgreen",linetype=2) +
  geom_line(aes(y=shaved_pop_upb),colour="forestgreen",linetype=2) +
  facet_wrap(~age_group,scales="free",ncol=5) +
  scale_x_date(date_labels = "%e %b\n%Y",
               minor_breaks = NULL,
               expand=expansion(0,c(0,2.5))) +
  labs(x=NULL,y="Projected population")
ggsave(suppfigpop,file="figures/suppfigpop.png")

