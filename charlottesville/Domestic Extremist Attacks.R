library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(viridis)


# download "globalterrorismdb_0617dist.xlsx" GTD file from:
# https://www.start.umd.edu/gtd/


# load data
f.name <- "globalterrorismdb_0617dist.xlsx"
f.dir <- "~/Dropbox/AWS/"
df.gtd <- read_excel(file.path(f.dir, f.name, fsep=""))


# get subset
df.usa.gtd <- df.gtd %>%
  filter(country_txt=="United States") %>%
  filter(iyear > 1989) %>%
  filter(nkill > 0)

# create filters
non.rw <- c("Palestinians",
            "Medellin Drug Cartel",
            "Tontons Macoutes",
            "Anti-Technology extremists", #unabomber
            "Anti-Israeli extremists", #LAX shooter
            "Anti-white" # Dallas gunman
            )

def.ms <- c("Al-Qaida",
            "Jamaat-al-Fuqra",
            "Muslim extremists",
            "Jihadi-inspired extremists"            
            )

def.rw <- c("Anti-Abortion extremists",
            "Anti-Government extremists",
            "Anti-Liberal extremists",
            "Anti-Muslim extremists",
            "Anti-Semitic extremists",
            "Minutemen American Defense",
            "Neo-Nazi extremists",
            "Sons of the Gestapo",
            "Sovereign Citizen",
            "White extremists",
            "Army of God",
            "World Church of the Creator"
            )

df.usa.gtd$rw <- "Other"
df.usa.gtd$rw[df.usa.gtd$gname %in% def.ms] <- "Islamist"
df.usa.gtd$rw[df.usa.gtd$gname %in% def.rw] <- "Right Wing"

table(df.usa.gtd$rw)

df.plot <- df.usa.gtd %>%
  select(iyear, rw) %>%
  filter(rw != "Other")

df.plot$rw <- factor(df.plot$rw)

df.plot$rw <- ordered(df.plot$rw, levels = c("Islamist", "Right Wing"))


# uncomment to add line + legend entry for 2009
#threshold <- data.frame(x=2009, name="JF Turns 12")

p <- ggplot(df.plot, aes(iyear))  +
  
  # uncomment to add line + legend entry for 2009
  #geom_vline(aes(xintercept = x, color = name), lty="dotted", data = threshold) +

  geom_bar(aes(fill=rw)) + 
  ggtitle("Fatal Terror Attacks in the United States, 1990-2016")       +
  theme(legend.title=element_blank(),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color = "#dedede", size=0.1),
        panel.grid.minor = element_line(color = "#efefef"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color= "#999999")) + 
  scale_x_continuous(breaks=seq(1990,2016,2)) + 
  scale_y_continuous(breaks=seq(0, 12, 1)) + 
  scale_fill_manual(values=rev(viridis(2))) + 
  ylab("Number of Fatal Attacks") +
  xlab("Year")

p <- p +  labs(caption = "Data: Global Terrorism Dataset") 

p

ggsave("~/Dropbox/AWS/IMG/rw-muslim.jpeg", plot = p, scale = 1,
       width = 10, height = 6, dpi = 300)