library(plyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(gridExtra)

source("1. Sample Code/99_Utility_v3.R")

UK <- read.csv("IP_predictions.csv")

UK <- UK %>% mutate(day = yday(ymd), year = year(ymd), dow = wday(ymd, label = TRUE)) %>% 
  filter(year > 2014) %>% mutate(holy = dow %in% c("Sat", "Sun"))

# Times observed vs predicted
tmpDat_1 <- data.frame(x = rep(UK$day, 2), 
                     y = c(UK$MR.Ocat+0.33,
                           UK$MR.CNN-0.33), 
                     Model = as.factor(rep(c("MR-Ocat", "MR-CNN"), 
                                           each = length(UK$MR.Ocat))))

tmpDat_2 <- data.frame(x = rep(UK$day, 2), 
                       y = c(UK$HR.Gauss+0.33,
                             UK$HR.FCNN-0.33), 
                       Model = as.factor(rep(c("HR-Gauss", "HR-FCNN"), 
                                             each = length(UK$HR.Gauss))))

plTimes_1 <- ggplot(tmpDat_1, aes(x = x, y = y, group = Model, colour = Model)) +  theme_bw() +
  # scale_x_continuous(breaks = c(datMxTest$timeCount[1], datMxTest$timeCount[366], 1), 
  #                     labels = c("Jan2010", "Jan2011", "Dec2011")) + 
  geom_point(size = 1) + geom_point(data = data.frame(x = UK$day, y = UK$todFrom1, holy = as.factor(UK$holy)), 
                                      mapping = aes(x = x, y = y), inherit.aes = FALSE, size = 1) +
  theme_Publication(base_size=30) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal", 
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + 
  scale_y_continuous(breaks = seq(1,48,4)) + 
  xlab("Day of Year") + ylab("Time of day [half-hour]")

plTimes_2 <- ggplot(tmpDat_2, aes(x = x, y = y, group = Model, colour = Model)) +  theme_bw() +
  # scale_x_continuous(breaks = c(datMxTest$timeCount[1], datMxTest$timeCount[366], 1), 
  #                     labels = c("Jan2010", "Jan2011", "Dec2011")) + 
  geom_point(size = 1) + geom_point(data = data.frame(x = UK$day, y = UK$todFrom1, holy = as.factor(UK$holy)), 
                                    mapping = aes(x = x, y = y), inherit.aes = FALSE, size = 1) +
  theme_Publication(base_size=30) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal", 
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) + 
  scale_y_continuous(breaks = seq(1,48,4)) + 
  xlab("Day of Year") + ylab("Time of day [half-hour]")

ALL <- read.csv("1. Sample Code/1. Data/UK.csv")

set.seed(5252)
ALL <- ALL %>% mutate(day = yday(date)) %>% 
  filter(Annee==2015) %>%
  mutate(Winter = day < 90 | day > 293) %>%
  mutate(Season = revalue(as.factor(Winter), c("FALSE" = "Summer", "TRUE" = "Winter"))) %>%
  group_by(day) %>% mutate(strange = !Winter & which.max(LOAD) > 30, 
                           small = abs(max(LOAD[Instant > 30])-max(LOAD[Instant <= 30])) < 0.05) %>% 
  ungroup()

ALL$LOAD[ALL$Winter == TRUE] <- ALL$LOAD[ALL$Winter == TRUE] + 1.5
profs <- ALL %>% ggplot(aes(x = Instant, y = LOAD*10, group = day, linetype = Season)) + geom_line(alpha = 0.2) + 
  geom_line(data = ALL[ALL$small == TRUE, ],
            mapping = aes(x = Instant, y = LOAD*10, group = day, colour = I("blue")),
            inherit.aes = FALSE,
            alpha = 0.3) +
  theme_Publication(base_size=30) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  xlab("Time of day") + 
  ylab("Demand [GW]")

pdf(file = "1. Sample Code/3. Plots/ocat-1.pdf", width = 15, height = 8)
grid.arrange(grobs = list(plTimes_1, plTimes_2), ncol = 2)
dev.off()

pdf(file = "1. Sample Code/3. Plots/ocat-2.pdf", width = 11, height = 10)
profs
dev.off()

