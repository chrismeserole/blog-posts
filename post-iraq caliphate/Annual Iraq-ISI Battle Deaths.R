library(dplyr)
library(tidyr)
library(mc2d)
library(ggplot2)

url <- "http://ucdp.uu.se/downloads/brd/ucdp-brd-dyadic-171.csv"
df.iraq <- read.csv(url)

df.clean <- df.iraq %>%
  filter(DyadID==524) %>%
  filter(Year < 2014) %>%
  select(Year, bdLow, bdBest, bdHigh)

# the data has high, low, and best estimates
# we're going to draw from PERT distribution based on each

M <- df.clean[, 2:4]
n <- 250
draws <- t(round(apply(M, 1, function(x) 
  rpert(n=n, min=x[1], mode=x[2], max=x[3], shape=4)),0))

df.draws <- data.frame(Year=df.clean$Year, draws)  

df.plot <- df.draws %>% 
  gather(DrawNum, Estimate, X1:X250) %>%
  group_by(Year)

ggplot(df.plot, aes(x = Year, y = Estimate, colour=DrawNum)) + 
  ggtitle("Figure 1: Annual Battle Deaths in Iraq between Iraqi Forces and AQI") +
  theme(legend.position="none",
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color = "#dedede", size=0.1),
        panel.grid.minor = element_line(color = "#efefef"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5)) + 
  geom_line(alpha=0.025) + 
  scale_color_manual(values=rep(c("red"),n)) + 
  scale_y_continuous(breaks=seq(0,4500,500), limits = c(0, 4500)) +
  scale_x_continuous(breaks=seq(2004,2013,1)) + 
  ylab("Estimated Deaths") 

