#Please contact authors about data availability
library(stringr)
load("../data/weibo_september_2014_sample10000.RData")
hist(as.Date(weibo.sample$created_at), breaks='day')

weibo.sample$lat <- as.numeric(sapply(as.character(weibo.sample$geo.coordinates1), function (x)
    gsub('\"', '', x)))
weibo.sample$long <- as.numeric(sapply(as.character(weibo.sample$geo.coordinates2), function (x)
    gsub('\"', '', x)))

weibo <-
    weibo.sample[as.Date(weibo.sample$created_at)<as.Date("2014-09-29")&
                                        weibo.sample$long>115 &
                            weibo.sample$long<120 &
                               weibo.sample$lat>38.5 &
                                  weibo.sample$lat<40,]

dim(weibo)
summary(as.Date(weibo$created_at))

load("../data/CN_2014-01-01_2015-12-31_short.RData")

#Language difference
prop.table(table(weibo.sample$user.lang[!duplicated(weibo.sample$user.id)])[table(weibo.sample$user.lang[!duplicated(weibo.sample$user.id)])>1])
sort(prop.table(table(tolower(DATA$user_lang[!duplicated(DATA$user_id_str)]))))
sum(str_detect(DATA$user_lang[!duplicated(DATA$user_id_str)], "zh"), na.rm=T)/length(unique(DATA$user_id_str))


#Condition on Chinese and geography
twitter <- DATA[as.Date(DATA$date)<as.Date("2014-09-29") &
                    as.Date(DATA$date)>as.Date("2014-08-31") &
                        DATA$coordinates_lat>115 &
                            DATA$coordinates_lat<120 &
                               DATA$coordinates_long>38.5 &
                                  DATA$coordinates_long<40
               & str_detect(tolower(DATA$user_lang), "zh"),]
dim(twitter)
summary(as.Date(twitter$date))
set.seed(01234)




#Geography plot
#Twitter map
library(ggplot2)
library(grid)
library(mapdata)

twittergeo <- twitter[!is.na(twitter$coordinates_lat),]
weibogeo <- weibo[!is.na(weibo$lat),]
twittergeo <- twittergeo[sample(1:nrow(twittergeo),nrow(weibogeo)),]

points <- data.frame(x=as.numeric(as.character(twittergeo$coordinates_lat)),
                                          y=as.numeric(as.character(twittergeo$coordinates_long)))
points2 <- data.frame(x=as.numeric(as.character(weibogeo$long)),
                                          y=as.numeric(as.character(weibogeo$lat)))

#### TWITTER

map.data <- map_data("world")
g <- ggplot(map.data) + geom_map(aes(map_id = region), map =
                                   map.data, fill = "white",
                               color =
                                   "grey20", size = 0.25) +
                                       expand_limits(x =
                                                            map.data$long, y = map.data$lat) +
                                                                theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                                                      axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
                                                                      panel.grid.major = element_blank(), plot.background = element_blank(),
                                                                      plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))


library(MASS)
## pdf("../figs/BeijingAreaTwitter2_for_presentation.pdf",width=4,height=4)
## jpeg("../figs/BeijingAreaTwitter2.jpg", quality=200)
## set smoothing to Twitter parameters
print(g + labs(title="Twitter") + theme(plot.title = element_text(size=24)) + ylim(38.5,40) + xlim(115, 120) +
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
print(g + labs(title="Twitter") + theme(plot.title = element_text(size=24))+ geom_point(data = points,
             aes(x = x, y = y), size = 1, alpha = 2/5, color = "darkblue") + ylim(38.5,40) + xlim(115, 120) +
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
print(g + labs(title="Twitter") + theme(plot.title = element_text(size=24))+ geom_point(data = points,
             aes(x = x, y = y), size = 1, alpha = 0.8, color = "darkblue") + ylim(38.5,40) + xlim(115, 120) + stat_density2d(data = points, aes(x = x, y = y), n=500, alpha=0.8, color="yellow", h=c(bandwidth.nrd(points$x), bandwidth.nrd(points$y)))+
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
## dev.off()

#### WEIBO

map.data <- map_data("world")
g2 <- ggplot(map.data) + geom_map(aes(map_id = region), map =
                                   map.data, fill = "white",
                               color =
                                   "grey20", size = 0.25) +
                                       expand_limits(x =
                                                            map.data$long, y = map.data$lat) +
                                                                theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                                                      axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
                                                                      panel.grid.major = element_blank(), plot.background = element_blank(),
                                                                      plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

## jpeg("../figs/BeijingAreaWeibo2.jpg", quality=200)
## pdf("../figs/BeijingAreaWeibo2_for_presentation.pdf",width=4,height=4)
print(g2 + labs(title="Weibo") + theme(plot.title = element_text(size=24)) + ylim(38.5,40) + xlim(115, 120) +
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      ## annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
print(g2 + labs(title="Weibo") + theme(plot.title = element_text(size=24)) + geom_point(data = points2,
                                                                          aes(x = x, y = y), size = 1, alpha = 2/5, color = "darkred") + ylim(38.5,40) + xlim(115, 120) +
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      ## annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
print(g2 + labs(title="Weibo") + theme(plot.title = element_text(size=24)) + geom_point(data = points2,
                                                                          aes(x = x, y = y), size = 1, alpha = 2/5, color = "darkred") + ylim(38.5,40) + xlim(115, 120) + stat_density2d(data = points2, aes(x = x, y = y), n=500, alpha=0.8, color="yellow", h=c(bandwidth.nrd(points$x), bandwidth.nrd(points$y)))+
## annotate("text", x = 116.38, y = 39.9167, label = "Beijing", color = "purple") +
      annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      ## annotate("text", x = 116.38, y = 39.9167, label = 'bolditalic("Beijing")', color = "black", parse=T) +
      annotate("text", x = 117.18, y = 39.1333, label = 'bolditalic("Tianjin")', parse=T)+ annotate("text", x = 116.52, y = 39.52, label = 'bolditalic("Langfang")', parse=T)+ annotate("text", x = 115.46, y = 38.86, label = 'bolditalic("Baoding")', parse=T)+ annotate("text", x = 118.13, y = 39.61, label = 'bolditalic("Tangshan")', parse=T) + annotate("text", x = 119.59, y = 39.93, label = 'bolditalic("Qinhuangdao")', parse=T)
      )
## dev.off()
