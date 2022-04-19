

# Created 19.04.2022

# Compare Poissons with overdispersion

#############################################################


library(dplyr)
library(fastDummies)
library(tidyr)
library(ggplot2)
library(cowplot)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/savepoint/")


res <- lapply(c("combined_samples_trun_temperature_corrected", "combined_samples_trun_temperature_corrected_OV", "combined_samples_trun_temperature_corrected_OV_0.01", "combined_samples_trun_temperature_corrected_OV_0.001"), readRDS)

listloop <- list()
model <- c("correct", "OV_0.1", "OV_0.01", "OV_0.001")
for(i in 1:4){
  
  res[[i]]$age_group$model <- model[i]
  res[[i]]$canton_name$model <- model[i]
  res[[i]]$phase$model <- model[i]
  res[[i]]$Total$model <- model[i]
  
  listloop[[i]] <- res[[i]]
}


# age
rm <- which(is.na(colnames(listloop[[4]]$age_group)))
age.res <- rbind(listloop[[1]]$age_group, listloop[[2]]$age_group[, -rm], listloop[[3]]$age_group[, -rm], listloop[[4]]$age_group[, -rm])
data_long <- gather(age.res, beta, values, `beta[1]:age_group_0-39`:`beta[10]:age_group_80+`, factor_key=TRUE)

data_long %>% select(model, beta, values) %>% group_by(model, beta) %>% 
  summarize(med = median(values), 
            LL = quantile(values, probs = 0.025), 
            UL = quantile(values, probs = 0.975)) %>% 
  mutate(model = factor(model, levels = c("correct", "OV_0.1", "OV_0.01", "OV_0.001"))) %>% 
  arrange(beta, model) -> data.long.age

data.long.age$betatype = rep(c("exp", "lab"), each = 20)
data.long.age$ageg <- 
  factor(gsub("age_group_", "", gsub(".*:","",data.long.age$beta)), 
         levels = c("0-39", "40-59", "60-69", "70-79", "80+"))

data.long.age %>% group_by(betatype) %>% 
  arrange(beta) %>% mutate(x = 1:n()) -> data.long.age

nams <- data.long.age[!duplicated(data.long.age$ageg),]


data.long.age %>% 
  ggplot() + geom_point(aes(x = x, y = med, col = model), size = 1.3) +
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL, col = model), width = 0) + 
  scale_x_continuous(breaks = nams$x, labels = nams$ageg) + 
  facet_grid(vars(betatype), scales = "free_y") +
  geom_hline(yintercept = 1, col = "red", linetype = 2) + 
  theme_bw() + ylab("median and 95%CrI") + 
  scale_color_viridis_d(begin = 0, end = 0.85) + 
  xlab("age groups") -> p1







# canton
rm <- which(is.na(colnames(listloop[[4]]$canton_name)))
canton.res <- rbind(listloop[[1]]$canton_name, listloop[[2]]$canton_name[, -rm], listloop[[3]]$canton_name[, -rm], listloop[[4]]$canton_name[, -rm])
data_long <- gather(canton.res, beta, values, `beta[1]:canton_name_Aargau`:`beta[52]:canton_name_Zürich`, factor_key=TRUE)

data_long %>% select(model, beta, values) %>% group_by(model, beta) %>% 
  summarize(med = median(values), 
            LL = quantile(values, probs = 0.025), 
            UL = quantile(values, probs = 0.975)) %>% 
  mutate(model = factor(model, levels = c("correct", "OV_0.1", "OV_0.01", "OV_0.001"))) %>% 
  arrange(beta, model) -> data.long.canton


data.long.canton$betatype = rep(c("exp", "lab"), each = 208/2)
data.long.canton$cantons <- 
  as.factor(gsub("canton_name_", "", gsub(".*:","",data.long.canton$beta)))

data.long.canton %>% group_by(betatype) %>% 
  arrange(beta) %>% mutate(x = 1:n()) -> data.long.canton

nams <- data.long.canton[!duplicated(data.long.canton$cantons),]



data.long.canton %>% 
  ggplot() + geom_point(aes(x = x, y = med, col = model), size = 1.3) +
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL, col = model), width = 0) + 
  scale_x_continuous(breaks = nams$x, labels = nams$cantons) + 
  facet_grid(vars(betatype), scales = "free_y") +
  geom_hline(yintercept = 1, col = "red", linetype = 2) + 
  theme_bw() + ylab("median and 95%CrI") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_color_viridis_d(begin = 0, end = 0.85) + 
  xlab("cantons") -> p2





# phase
rm <- which(is.na(colnames(listloop[[4]]$phase)))
phase.res <- rbind(listloop[[1]]$phase, listloop[[2]]$phase[, -rm], listloop[[3]]$phase[, -rm], listloop[[4]]$phase[, -rm])
data_long <- gather(phase.res, beta, values, `beta[1]:phase_1`:`beta[12]:phase_6`, factor_key=TRUE)

data_long %>% select(model, beta, values) %>% group_by(model, beta) %>% 
  summarize(med = median(values), 
            LL = quantile(values, probs = 0.025), 
            UL = quantile(values, probs = 0.975)) %>% 
  mutate(model = factor(model, levels = c("correct", "OV_0.1", "OV_0.01", "OV_0.001"))) %>% 
  arrange(beta, model) -> data.long.phase



data.long.phase$betatype = rep(c("exp", "lab"), each = 48/2)
data.long.phase$phase <- 
  as.factor(gsub("phase_", "", gsub(".*:","",data.long.phase$beta)))

data.long.phase %>% group_by(betatype) %>% 
  arrange(beta) %>% mutate(x = 1:n()) -> data.long.phase

nams <- data.long.phase[!duplicated(data.long.phase$phase),]



data.long.phase %>% 
  ggplot() + geom_point(aes(x = x, y = med, col = model), size = 1.3) +
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL, col = model), width = 0) + 
  scale_x_continuous(breaks = nams$x, labels = nams$phase) + 
  facet_grid(vars(betatype), scales = "free_y") +
  geom_hline(yintercept = 1, col = "red", linetype = 2) + 
  theme_bw() + ylab("median and 95%CrI") + 
  scale_color_viridis_d(begin = 0, end = 0.85) + 
  xlab("phase") -> p3






# total
rm <- 3
total.res <- rbind(listloop[[1]]$Total, listloop[[2]]$Total[, -rm], listloop[[3]]$Total[, -rm], listloop[[4]]$Total[, -rm])
data_long <- gather(total.res, beta, values, `beta[1]:exp`:`beta[2]:lab`, factor_key=TRUE)

data_long %>% select(model, beta, values) %>% group_by(model, beta) %>% 
  summarize(med = median(values), 
            LL = quantile(values, probs = 0.025), 
            UL = quantile(values, probs = 0.975)) %>% 
  mutate(model = factor(model, levels = c("correct", "OV_0.1", "OV_0.01", "OV_0.001"))) %>% 
  arrange(beta, model) -> data.long.total

data.long.total$betatype = rep(c("exp", "lab"), each = 8/2)
data.long.total$phase <- 
  as.factor(gsub(".*:","",data.long.total$beta))

data.long.total %>% group_by(betatype) %>% 
  arrange(beta) %>% mutate(x = 1:n()) -> data.long.total

data.long.total %>% 
  ggplot() + geom_point(aes(x = x, y = med, col = model), size = 1.3) +
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL, col = model), width = 0) + 
  facet_grid(vars(betatype), scales = "free_y") +
  geom_hline(yintercept = 1, col = "red", linetype = 2) + 
  theme_bw() + ylab("median and 95%CrI") + 
  xlab("Total") + 
  scale_color_viridis_d(begin = 0, end = 0.85) -> p4


p1
p2
p3
p4



#############################################################
#############################################################
#############################################################
#############################################################





