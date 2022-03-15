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


newdata.travel <- data.frame(RL = seq(from = 110, to = 160 ,length=100))# predictions and confidence intervals
pm <- predict(m.dr.travel, newdata=newdata.travel, interval="confidence") # new data with predictions
newdata.travel$p <- pm[,1]
newdata.travel$pmin <- pm[,2]
newdata.travel$pmax <- pm[,3]
newdata.travel$traveling01 <- 'travelling'

## not traveling -----
m.dr.nottravel <- drm(sev_avail_scaled ~ RL, data = brsdata %>% filter(traveling01 == 'not travelling'), fct = LL.3())
summary(m.dr.nottravel)


newdata.notravel <- data.frame(RL = seq(from = 110, to = 160 ,length=100))# predictions and confidence intervals
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
  xlab("Received Sound Levels (dB re 1 µ Pa)") +
  ylab('Probability of response') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

# output -----
ggsave(plot = p.dose.response, filename = 'output/dose_response_plot.png', height = 8, width = 13)
