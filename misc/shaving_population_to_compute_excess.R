# Let's consider 10 weeks of data for some subgroup in 2020 with population 1000:
nweek = 10
pred_pop_without_covid = rep(1000,nweek)

# Let's say that 1% of this population are expected to die each week:
baseline_mortality = 0.01

# Assume that there are 200 additional deaths due to covid in the 2nd week:
covid_deaths = c(0,200,rep(0,nweek-2))

# This unexpectedly reduces the population after week 2 (note that the impact of 
# exp_deaths on population is already accounted for in the predictions of population, 
# as are the impacts of immigration, ageing and births):
actual_pop_with_covid = pred_pop_without_covid - cumsum(lag(covid_deaths,default=0))

# Put everything together:                 
dat = tibble(pred_pop = pred_pop_without_covid,
             actual_pop = actual_pop_with_covid) %>% 
  mutate(exp_deaths = pred_pop * baseline_mortality,
         obs_deaths = actual_pop * baseline_mortality + covid_deaths)

# Now compute excess as we did initially;
dat = dat %>% 
  mutate(naive_excess = obs_deaths - exp_deaths)

# If we sum the excess over the period, we underestimate the actual excess by:
sum(dat$naive_excess)
sum(covid_deaths)

# We want to shave the population as we go to account for the impact of covid waves on the population:
dat$shaved_pop = NA
dat$corr_excess = NA

dat$shaved_pop[1] = dat$pred_pop[1]
dat$corr_excess[1] = dat$obs_deaths[1] - dat$exp_deaths[1]
for(i in 2:nweek) {
  dat$shaved_pop[i] = dat$pred_pop[i] - sum(dat$corr_excess,na.rm=TRUE)
  dat$corr_excess[i] = dat$obs_deaths[i] - (dat$exp_deaths[i]/dat$pred_pop[i])*dat$shaved_pop[i]
}

# Now we get the excess right:
sum(dat$corr_excess)
sum(covid_deaths)
dat
