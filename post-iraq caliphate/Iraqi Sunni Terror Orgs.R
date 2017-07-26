library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# downloaded GTD from https://www.start.umd.edu/gtd/

file.path <- '/PATH/TO/globalterrorismdb_0617dist.xlsx'
df.gtd <- read_excel(file.path)
names(df.gtd)

# get subset
df.iraq.gtd <- df.gtd %>%
  filter(country_txt=="Iraq") %>%
  filter(iyear > 2003) %>%
  filter(nkill > 4) 

# filter miscellaneous names
df.iraq.gtd <- df.iraq.gtd %>%
  filter(gname != "Unknown") %>%
  filter(gname != "Gunmen") %>%
  filter(gname != "Muslim extremists") %>%
  filter(gname != "Jihadist Soldiers") %>%
  filter(gname != "Sunni Supporters") %>%
  filter(gname != "Sunni Muslim extremists") %>%
  filter(gname != "Iraqi extremists") %>%
  filter(gname != "Iraqi Sunni extremists") %>%
  filter(gname != "Iraqi Sunni Extremists") %>%
  filter(gname != "Islamic Companies") 

# filter shia names
df.iraq.gtd <- df.iraq.gtd %>%
  filter(gname != "Asa'ib Ahl al-Haqq") %>%       
  filter(gname != "Mukhtar Army") %>%             
  filter(gname != "Kata'ib Hezbollah") %>%
  filter(gname != "Shia Muslim extremists") %>%
  filter(gname != "Mahdi Army") %>%
  filter(gname != "Mahdi Army")


# match Islamic State groups
isi <- "Islamic State of Iraq (ISI)"
isil <- "Islamic State of Iraq and the Levant (ISIL)"
aqi <- "Al-Qaida in Iraq"
tj <- "Tawhid and Jihad"
df.iraq.gtd <- df.iraq.gtd %>%
  mutate(gname=replace(gname, gname==isil, isi)) %>%
  mutate(gname=replace(gname, gname==aqi, isi)) %>%
  mutate(gname=replace(gname, gname==tj, isi )) 


## create dataset of unique actors by year
df.plot <- data.frame(year=NA, group=NA)
for(i in 2004:2016){
  df.plot <- rbind(df.plot, 
                   data.frame(year = i, 
                              group = unique(df.iraq.gtd$gname[df.iraq.gtd$iyear==i])
                   ))
}
df.plot <- df.plot[complete.cases(df.plot),]


# create dummy for isis/aqi
df.plot$isi <- "ISIS/AQI"
df.plot$isi[df.plot$group != "Islamic State of Iraq (ISI)"] <- "Sunni Org"
df.plot$isi <- ordered(df.plot$isi, levels = c("Sunni Org","ISIS/AQI"))


# plot
ggplot(df.plot, aes(year))  + geom_bar(aes(fill=isi)) + 
  ggtitle("Figure 2: Active Sunni Terrorist Organizations in Iraq") +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(margin = margin(l = 1 , r = 2 )),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color = "#dedede", size=0.1),
        panel.grid.minor = element_line(color = "#efefef"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5),) + 
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) + 
  scale_x_continuous(breaks=seq(2004,2016,1)) + 
  ylab("Number of Sunni Organizations") +
  xlab("Year")