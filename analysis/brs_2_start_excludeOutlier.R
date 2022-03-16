# libraries ----
library(tidyverse)
library(ggplot2)
# library(data.table)
# library(nlme)
library(drc)

theme_set(theme_light())

# functions -----
source('R/as.numeric.factor.R')
# read data -----
brsdata <- read.csv('data/datset for Alejandro.csv')

dim(brsdata)
brsdata <-
  brsdata %>%
  filter(traveling01 %in% c('not travelling', 'travelling')) %>%
  droplevels()

brsdata <-
  brsdata %>%
  mutate(RL = as.numeric.factor(RL))

ggplot(data = brsdata , aes(x = RL, y = sev_avail, color = traveling01)) +
  geom_point() +
  xlab('Received level') +
  xlim(100, 160) +
  ylab('Severity weighted by availability') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

# remove outlier ------
brsdata <-
  brsdata %>%
  filter(sev_avail < 4.7)

# rescale data -----
brsdata <- brsdata %>%
  mutate(sev_avail_scaled = sev_avail/max(sev_avail) )



brsdata[which(brsdata$sev_avail_scaled == 1), 'sev_avail_scaled'] <- 0.999


brsdata %>%
  select(RL, sev_avail, sev_avail_scaled, traveling01) %>% str()
ggplot(data = brsdata , aes(x = RL, y = sev_avail_scaled, color = traveling01)) +
  geom_point() +
  xlab('Received level') +
  # xlim(100, 160) +
  # scale_x_continuous(limits = c(100, 160)) +
  ylab('Severity weighted by availability') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

# fit logistic curve -----
# glm(sev_avail_scaled ~ RL ,
#    data = brsdata,
#    family = poisson(link = "logit"))

# y=a/(1+be−cx)

# m.log<- nls(sev_avail ~ a/(1+be−cx),start=list(a=120,b=110,c=0.064))

# fit asymptotic curve -----
# y=a−be−cx

# dose-response ------
## travel -----
m.dr.travel <- drm(sev_avail_scaled ~ RL, data = brsdata %>% filter(traveling01 == 'travelling'), fct = LL.3())
summary(m.dr.travel)


newdata.travel <- data.frame(RL = seq(from = 100, to = 160 ,length=100))# predictions and confidence intervals
pm <- predict(m.dr.travel, newdata=newdata.travel, interval="confidence") # new data with predictions
newdata.travel$p <- pm[,1]
newdata.travel$pmin <- pm[,2]
newdata.travel$pmax <- pm[,3]
newdata.travel$traveling01 <- 'travelling'

## not traveling -----
m.dr.nottravel <- drm(sev_avail_scaled ~ RL, data = brsdata %>% filter(traveling01 == 'not travelling'), fct = LL.3())
summary(m.dr.nottravel)


newdata.notravel <- data.frame(RL = seq(from = 100, to = 160 ,length=100))# predictions and confidence intervals
pm <- predict(m.dr.nottravel, newdata=newdata.notravel, interval="confidence") # new data with predictions
newdata.notravel$p <- pm[,1]
newdata.notravel$pmin <- pm[,2]
newdata.notravel$pmax <- pm[,3]
newdata.notravel$traveling01 <- 'not travelling'

# plot ----
newdata <- bind_rows(newdata.travel, newdata.notravel)

p.dose.response <-
  ggplot(brsdata, aes(x = RL, y = sev_avail_scaled, color = traveling01, fill = traveling01)) +
  geom_point() +
  geom_ribbon(data=newdata, aes(x=RL, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(data=newdata, aes(x=RL, y=p)) +
  xlim(100,155)+
  # ylim(0, 1.1)+
  xlab("Received Sound Levels (dB re 1 µ Pa)") +
  ylab('Probability of response') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

ed50_notravel <- ED(m.dr.nottravel, c(50), interval = "delta")
ed50_notravel <- ed50_notravel[1,1]
ed50_travel <- ED(m.dr.travel, c(50), interval = "delta")
ed50_travel <- ed50_travel[1,1]

ggplot_build(p.dose.response)$data

p.dose.response_ed50 <- p.dose.response +
  geom_vline(xintercept = ed50_notravel, lty = 2, color = '#F8766D') +
  geom_vline(xintercept = ed50_travel, lty = 2, color = '#00BFC4')  +
  annotate(geom="text", x=118.5, y=.94, label="ED50",
           color="black", angle = 90)
# parameters ------

ED(m.dr.nottravel, c(5, 10, 50, 75, 90), interval = "delta")
ED(m.dr.travel, c(5, 10, 50, 75, 90), interval = "delta")
# output -----
ggsave(plot = p.dose.response, filename = 'output/dose_response_plot_excludeoutlier.png', height = 8, width = 13)
ggsave(plot = p.dose.response_ed50, filename = 'output/dose_response_plot_excludeoutlier_ed50.png', height = 8, width = 13)

