library(twitteR)
library(ggplot2)
library(rsvg)
library(magick)
library(lubridate)
library(dplyr)
library(scales)
library(tidyr)
library(ggthemes)


##
##  Set up twitter
##
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


##
## globals
##

project.path <- "/path/to/misinformation-directory"
image.path <- file.path(project.path, "img")

falsetweet <- image_resize(image_read(file.path(image.path, "FalseTweet.png")), "50%")
truetweet <- image_resize(image_read(file.path(image.path, "TrueTweet.PNG")), "50%")
linefalse <- image_read(file.path(image.path, "linefalse.png"))
linetrue <- image_read(file.path(image.path, "linetrue.PNG"))

df.skeleton  <- data.frame(interval = seq(ymd_hms('2018-04-23 18:35:00'), 
                                          by = '1 min',length.out=(24*60*2)))


##
## Get false tweet data
##
txt.false <- "Witness to truck ramming into pedestrians tells local Toronto TV station"
tw.false <- searchTwitteR(txt.false, n=3200, lang='en', since="2018-04-23", until="2018-04-25", resultType = "recent")
df.tw.false <- twListToDF(tw.false)

df.sort <- df.tw.false %>%
  filter(created < "2018-04-23 19:00:00") %>%
  arrange(created)

df.sort[1:100, c("created", "screenName")]

df.count.false <- df.tw.false %>% 
  mutate(interval = floor_date(created, unit="hour")+minutes(floor(minute(created)))) %>% 
  group_by(interval) %>%
  summarize(count = n())

df.count.false <- merge(df.count.false, df.skeleton, by="interval", all=T)
df.count.false[is.na(df.count.false)] <- 0
df.count.false <- df.count.false %>%
  mutate(cumsum = cumsum(count))

##
## Get true tweet data
##
txt.true <- "Another eye witness to the Yonge and Sheppard incident describes"
tw.true <- searchTwitteR(txt.true, n=3200, lang='en', since="2018-04-23", until="2018-04-25", resultType = "recent")

df.tw.true <- twListToDF(tw.true)

df.count.true <- df.tw.true %>% 
  mutate(interval = floor_date(created, unit="hour")+minutes(floor(minute(created)))) %>% 
  group_by(interval) %>%
  summarize(count = n())

df.count.true <- merge(df.count.true, df.skeleton, by="interval", all=T)
df.count.true[is.na(df.count.true)] <- 0
df.count.true <- df.count.true %>%
  mutate(cumsum = cumsum(count))


##
## Merge all the datas
##
df.final <- merge(df.count.true, df.count.false, by="interval", all=T)
save(df.final,file=file.path(project.path, "Tweet Data.Rda"))

load(file.path(project.path, "Tweet Data.Rda"))

cutoff <- "2018-04-23 20:35:00"
attr(cutoff, 'tzone') = "US/Eastern"

df.plot <- df.final %>%
  gather(tweet, total, cumsum.y, cumsum.x) %>%
  filter(interval < cutoff)
attr(df.plot$interval, 'tzone') = 'US/Eastern'

df.plot$tweet <- factor(df.plot$tweet, labels=c("Accurate Tweet", "False Tweet"))
df.plot$tweet <- relevel(df.plot$tweet, "False Tweet")

##
## Subset the data we need for gif
##
full.interval <- seq(ymd_hms('2018-04-23 18:35:00'), by = '15 min',length.out=(22))
attr(full.interval, 'tzone') = 'US/Eastern'
titletimes <- format(strptime(full.interval, '%Y-%m-%d %H:%M:%S'), '%l:%M %p')
mylist <- list()
for(i in 1:length(full.interval)){
  mylist[[i]] <- df.plot %>%
    filter(interval <= full.interval[i])
  mylist[[i]]$total[mylist[[i]]$total == 0] <- NA
  mylist[[i]]$titlz <- paste("Total Retweets by", trimws(as.character(titletimes[i])),"(EST) on April 23, 2018\n\n")
}
names(mylist) <- full.interval
mylist$`2018-04-23 14:35:00`$total[1] <- 0

##
## Create the gif (note: i saved manually using right-click)
##
img <- image_graph(960, 480, res = 96)

out <- lapply(mylist, function(data){
  p <- ggplot(data, aes(interval, total, color=tweet)) + geom_line(size=1) +
    scale_y_continuous(name="Total Retweets", breaks=seq(0, 1400, 200), limits=c(0,1400)) +
    xlab("Time") +
    theme_economist() + 
    labs(title = "How Misinformation Spreads on Twitter", subtitle = data$titlz) +
    theme(plot.margin=unit(c(15,350,15,15),"pt"),
          axis.title=element_text(size=14)) +
    theme(legend.title=element_blank(),
          legend.position="none") +
    scale_x_datetime(labels = date_format("%l:%M %p", tz="US/Eastern"),
                     limits = c(min(full.interval), max(full.interval))) 
    
  print(p)
})
# note: had to manually save to img/ directory

##
## Create the final gif
##
img <- image_graph(960, 480, res = 300)

image_read(file.path(image.path, "6-hour-15min.gif")) %>%
  
  image_apply( function(banana){
    image_composite( banana, falsetweet, offset = "+635+102")
  }) %>%
  
  image_apply( function(banana){
    image_composite( banana, truetweet, offset = "+635+293")
  }) %>%
  
  image_apply( function(banana){
    image_composite( banana, linefalse, offset = "+527+150")
  }) %>%
  
  image_apply( function(banana){
    image_composite( banana, linetrue, offset = "+529+343")
  }) %>%
  
  image_annotate("False", size = 16, color = "Black", location="+561+140") %>%
  image_annotate("Account", size = 16, color = "Black", location="+561+160") %>%
  
  image_annotate("Accurate", size = 16, color = "Black", location="+561+332") %>%
  image_annotate("Account", size = 16, color = "Black", location="+561+352") %>%

  image_annotate("@chrismeserole", size = 14, color = "gray", location="+822+445") %>%
    
  image_animate(fps = 4)

?image_animate

##
## Create the 24 hour plot
##
plot.cutoff <- "2018-04-24 1:35:00"
attr(plot.cutoff, 'tzone') = "US/Eastern"

df.plot <- df.final 
attr(df.plot$interval, 'tzone') = 'US/Eastern'

df.plot <- df.plot %>%
  gather(tweet, total, cumsum.y, cumsum.x) %>%
  filter(interval <= plot.cutoff)

df.plot$tweet <- factor(df.plot$tweet, labels=c("True Tweet", "False Tweet"))
df.plot$tweet <- relevel(df.plot$tweet, "False Tweet")

min.interval <- min(df.plot$interval)
max.interval <- max(df.plot$interval)

img <- image_graph(960, 480, res = 96)

ggplot(df.plot, aes(x=interval, y=total, color=tweet)) + 
  geom_line(size=1.5) +
  scale_y_continuous(name="Total Retweets", breaks=seq(0, 1600, 200), limits=c(0,1600)) +
  xlim(min.interval, max.interval) + 
  xlab("Time") +
  theme_economist() + 
  labs(title = "How Misinformation Spreads on Twitter", 
       subtitle="Total Retweets Over 24 Hours, Monday April 23 to Tuesday April 24") +
  theme(plot.margin=unit(c(15,350,15,15),"pt"),
        axis.title=element_text(size=14)) +
  theme(legend.title=element_blank(),
        legend.position="none")  + 
  scale_x_datetime(labels = date_format("%l:%M %p", tz="US/Eastern"),
                   breaks = date_breaks("4 hour"))
# note: ggsave() doesn't allow for same units as image_graph() above
# manually save image as 24-hour.png in img/ directory instead

image_read(file.path(image.path, "24-hour.png")) %>%
  
  image_composite( falsetweet, offset = "+635+88") %>%
  image_composite( linefalse, offset = "+535+140")%>%
  
  image_composite(truetweet, offset = "+635+293") %>%
  image_composite( linetrue, offset = "+537+343") %>%
  
  image_annotate("False", size = 16, color = "Black", location="+570+126") %>%
  image_annotate("Tweet", size = 16, color = "Black", location="+570+146") %>%
  
  image_annotate("True", size = 16, color = "Black", location="+570+332") %>%
  image_annotate("Tweet", size = 16, color = "Black", location="+570+352") %>%
  
  image_annotate("@chrismeserole", size = 14, color = "gray", location="+822+445")


##


