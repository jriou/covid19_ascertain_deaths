

# Created 22.03.2020

# Figure 1


####################################################################



library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(sf)
library(cowplot)

setwd("E:/Postdoc Imperial/Projects/COVID19 Greece/covid19_ascertain_deaths/")

samp = readRDS("savepoint/merged_samples.rds")
dat <- samp$samples_base
class(samp$samples_base$canton_name)

length(table(dat$canton_id))
length(table(dat$canton_name))

dat %>% filter(!(phase %in% 7)) -> dat
dat %>% group_by(canton_name, week, it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot



dat %>% 
  filter(it %in% 1) %>% 
  group_by(canton_name, week) %>% 
  summarize(deaths = sum(deaths)) %>% 
  left_join(dat2plot, .) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths) %>% 
  group_by(canton_name, week) %>% 
  summarize(relative_excess = median(relative_excess)) -> median_relative_excess

median_relative_excess$x <- as.numeric(as.factor(median_relative_excess$week))
median_relative_excess$y <- abs(as.numeric(as.factor(median_relative_excess$canton_name)) - 27)
median_relative_excess$median.rxs.cat <- cut(median_relative_excess$relative_excess*100, 
                               breaks = c(-200, 0, 10, 50, 100, 200, 1000), 
                               labels = c("0\u2264", "0-10", "10-50", "50-100", "100-200", ">200"), 
                               include.lowest = FALSE) 


cols_exp <- brewer.pal(n = 11, name = "RdBu")[c(6, 5:1)]
N <- length(unique(median_relative_excess$canton_name))
M <- length(unique(median_relative_excess$week))
# face.g <- c("bold", rep("plain", times = N))

phases <- readRDS("data/phases.rds")
phases$x <- as.numeric(as.factor(phases$week))
phases %>% filter(phase != 7) %>% group_by(phase) %>% summarise(min.x = min(x)) -> phase.x
phases %>% filter(phase != 7) %>% group_by(phase) %>% summarise(mean = mean(x)) -> phase.annotate

xlabs <- c(phases %>% 
             filter(phase != 7) %>% 
             group_by(phase) %>% 
             summarize(min.date = min(week)) %>% 
             pull(min.date), 
           phases %>% 
             filter(phase != 7) %>% 
             pull(week) %>% 
             max())


gPlot <- ggplot() +  
  # ylim(c(0, c(N+2))) + 
  xlim(c(0, M)) + 
  scale_fill_manual(values=cols_exp) + 
  theme_bw() + geom_raster(data = median_relative_excess, aes(x = x, y = y, fill = median.rxs.cat)) +
  scale_y_continuous(breaks = N:1,
                     expand = expansion(mult = c(0, 0)))  + 
  scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                     breaks = c(phase.x$min.x, max(phases %>% filter(phase != 7) %>% pull(x))), 
                     # labels = xlabs, 
                     labels = format(xlabs, "%d %b\n%Y")) + 
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    plot.margin = margin(0.7, 0, 0, 0, "cm"), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_line( size=.1, color="black" ),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE, 
    axis.text.y = element_text(size=7), 
    axis.text.x = element_text(size=7), 
    legend.spacing.y = unit(0, "mm"), 
    legend.spacing.x = unit(0, "mm"), 
    legend.title = element_blank(), 
    plot.title = element_text(size = 9, face = "bold", margin=margin(0,0,0,0)), 
    plot.subtitle = element_text(size = 7, face = "bold", margin=margin(0,0,0,0)), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.2, "cm"), 
    legend.text = element_text(size = 7)
  ) + ylab("") + xlab("") +
  geom_vline(xintercept = phase.x$min.x[-1], col = "black", size = .5, alpha = 1) + 
  annotate("label", x = phase.annotate$mean, y = 27.5, label = phase.annotate$phase, size = 2) + 
  coord_cartesian(ylim = c(0.5, N+0.5), clip = 'off')
gPlot


##
## and now maps by phase

shp <- read_sf("E:/Postdoc Imperial/Projects/COVID19 Greece/michela code/data/Switzerland/shp.shp")
plot(shp$geometry)

# here I will plot exceedance

dat %>% group_by(canton_id, phase, it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot

dat %>% 
  filter(it %in% 1) %>% 
  group_by(canton_id, phase) %>% 
  summarize(deaths = sum(deaths)) %>% 
  left_join(dat2plot, .) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths, 
         exceedance = relative_excess>0) %>% 
  group_by(canton_id, phase) %>% 
  summarize(exceedance = mean(exceedance)) -> excess_relative_excess
  

# and define the categories
excess_relative_excess$ex.prob.cat <- "(0.2, 0.8]"
excess_relative_excess$ex.prob.cat[excess_relative_excess$exceedance > 0.80] <- "(0.8, 1]"
excess_relative_excess$ex.prob.cat[excess_relative_excess$exceedance <= 0.20] <- "[0, 0.2]"
excess_relative_excess$ex.prob.cat <-
  factor(excess_relative_excess$ex.prob.cat, levels = c("[0, 0.2]", "(0.2, 0.8]", "(0.8, 1]"))
  
excess_relative_excess$ex.prob.cat <- cut(excess_relative_excess$exceedance, 
                                          breaks = c(-1, 0.05, 0.2, 0.8, 0.95, 1.1), 
                                          labels = c("[0, 0.05)", "[0.05, 0.2)", "[0.2, 0.8)", 
                                                     "[0.8, 0.95)", "[0.95, 1]"))


# Specify id.vars: the variables to keep but not split apart on

left_join(excess_relative_excess,
          shp,
          by = c("canton_id" = "ID_PE")
) -> shp2plot

shp2plot <- st_as_sf(shp2plot)

cols_exd <- brewer.pal(n = 11, name = "RdBu")
cols_exd <- c(cols_exd[c(2, 4, 6, 8, 10)])

ggplot(shp2plot) +
  geom_sf(col = "black", size = 0.1, aes(fill = ex.prob.cat)) + 
  scale_fill_manual(values=cols_exd[length(cols_exd):1], name = "", drop=FALSE) +
  facet_wrap(vars(phase), ncol = 3) + 
  theme_bw() +
  theme(text = element_text(size=8), 
        plot.title = element_text(face = "bold"),
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(colour = "grey"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.2, "cm"), 
        legend.position="bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> maps


png("savepoint/Fig1.png", width = 16, height = 16, res = 300, units = "cm")
plot_grid(gPlot, maps, labels = "AUTO", ncol = 1, rel_widths = c(1, 2))
dev.off()






##
## Another figure

# Here I will get relative excess as Figure 2



# Total

dat %>% group_by(it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot

dat2plot %>% mutate(deaths = dat %>% 
                      filter(it %in% 1) %>% 
                      summarize(deaths = sum(deaths)) %>% 
                      pull(deaths)) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths) -> median_relative_excess

  
datCrI <- data.frame(Type = "Overall", 
                     categories = NA, 
                     median = median(median_relative_excess$relative_excess)*100, 
                     LL = quantile(median_relative_excess$relative_excess, probs = 0.025)*100, 
                     UL = quantile(median_relative_excess$relative_excess, probs = 0.975)*100)


# by phase
dat %>% group_by(phase, it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot

dat %>% 
  filter(it %in% 1) %>% 
  group_by(phase) %>% 
  summarize(deaths = sum(deaths)) %>% 
  left_join(dat2plot, .) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths) %>% 
  group_by(phase) %>% 
  summarize(median = median(relative_excess)*100, 
            LL = quantile(relative_excess, probs = 0.025)*100, 
            UL = quantile(relative_excess, probs = 0.975)*100) -> excess_relative_excess



excess_relative_excess <- as.data.frame(cbind(Type = "phase", excess_relative_excess))
colnames(excess_relative_excess)[2] <- "categories"

datCrI <- rbind(datCrI, excess_relative_excess)


# by age
dat %>% group_by(age_group, it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot

dat %>% 
  filter(it %in% 1) %>% 
  group_by(age_group) %>% 
  summarize(deaths = sum(deaths)) %>% 
  left_join(dat2plot, .) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths) %>% 
  group_by(age_group) %>% 
  summarize(median = median(relative_excess)*100, 
            LL = quantile(relative_excess, probs = 0.025)*100, 
            UL = quantile(relative_excess, probs = 0.975)*100) -> excess_relative_excess



excess_relative_excess <- as.data.frame(cbind(Type = "age_group", excess_relative_excess))
colnames(excess_relative_excess)[2] <- "categories"

datCrI <- rbind(datCrI, excess_relative_excess)



# by canton
dat %>% group_by(canton, it) %>% summarize(exp_deaths = sum(exp_deaths)) -> dat2plot

dat %>% 
  filter(it %in% 1) %>% 
  group_by(canton) %>% 
  summarize(deaths = sum(deaths)) %>% 
  left_join(dat2plot, .) %>% 
  mutate(relative_excess = (deaths - exp_deaths)/exp_deaths) %>% 
  group_by(canton) %>% 
  summarize(median = median(relative_excess)*100, 
            LL = quantile(relative_excess, probs = 0.025)*100, 
            UL = quantile(relative_excess, probs = 0.975)*100) -> excess_relative_excess



excess_relative_excess <- as.data.frame(cbind(Type = "canton", excess_relative_excess))
colnames(excess_relative_excess)[2] <- "categories"

datCrI <- rbind(datCrI, excess_relative_excess)

datCrI$categories[is.na(datCrI$categories)] <- "Overall"
datCrI %>% group_by(Type) %>% mutate(x = as.numeric(as.factor(categories))) -> datCrI


datCrI %>% filter(Type %in% "Overall") %>% 
  ggplot() + 
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL), width = 0) + 
  geom_point(aes(x = x, y = median), size = 1) + 
  ylim(c(-20, 40)) + 
  scale_x_continuous(breaks = 1, labels = "Overall") + 
  theme_bw() + xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + 
  theme(text = element_text(size=8), 
        plot.title = element_text(face = "bold"),
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(colour = "grey"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.2, "cm"), 
        legend.position="bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> p1


datCrI %>% filter(Type %in% "phase") %>% 
  ggplot() + 
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL), width = 0) + 
  geom_point(aes(x = x, y = median), size = 1) + 
  ylim(c(-20, 40)) + 
  theme_bw() + xlab("") + ylab("")  + 
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + 
  theme(text = element_text(size=8), 
        plot.title = element_text(face = "bold"),
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(colour = "grey"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.2, "cm"), 
        legend.position="bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> p2


datCrI %>% filter(Type %in% "age_group") %>% 
  ggplot() + 
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL), width = 0) + 
  geom_point(aes(x = x, y = median), size = 1) + 
  scale_x_continuous(breaks = 1:5, labels = c("0-39", "40-59", "60-69", "70-79", "80+")) + 
  ylim(c(-20, 40)) + 
  theme_bw() + xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + 
  theme(text = element_text(size=8), 
        plot.title = element_text(face = "bold"),
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(colour = "grey"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.2, "cm"), 
        legend.position="bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> p3


datCrI %>% filter(Type %in% "canton") %>% 
  ggplot() + 
  geom_errorbar(aes(x = x, ymin = LL, ymax = UL), width = 0) + 
  geom_point(aes(x = x, y = median), size = 1) + 
  scale_x_continuous(breaks = 1:25, labels = datCrI$categories[-c(1:12)]) + 
  ylim(c(-20, 40)) + 
  theme_bw() + xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dotted") + 
  theme(text = element_text(size=8), 
        plot.title = element_text(face = "bold"),
        legend.key =
          element_rect(fill = 'white', color = "white", size = 0.1),
        legend.text = element_text(size = 7),
        legend.box.background = element_rect(colour = "grey"), 
        legend.background = element_blank(), 
        legend.spacing.y = unit(.10, "mm"), 
        legend.spacing.x = unit(0, "mm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.2, "cm"), 
        legend.position="bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> p4



plot_grid(p1, p2, p3, p4, nrow = 1, rel_widths = c(2, 3, 3, 6)) -> likeres


# datCrI %>% 
#   ggplot() + 
#   geom_errorbar(aes(x = x, ymin = LL, ymax = UL), width = 0) + 
#   geom_point(aes(x = x, y = median), size = 3) + facet_grid(cols = vars(Type), scale = "free_x")



png("savepoint/Fig1_up.png", width = 16, height = 20, res = 300, units = "cm")
plot_grid(likeres, maps, gPlot, labels = "AUTO", rel_widths = c(.8,2.2,1), ncol = 1)
dev.off()




png("savepoint/Fig1_up.png", width = 16, height = 16, res = 300, units = "cm")
plot_grid(likeres, gPlot, labels = "AUTO", rel_widths = c(.8,2.2),  ncol = 1)
dev.off()
