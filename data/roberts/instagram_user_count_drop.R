setwd("../data")

library(data.table)
## library(SpatialEpi)
library(stringr)

## ## this provides CN coordinates
## load("../data/latitude_longitude_population_agg_china_hk_only.RData")

sample.iso3 <- "CHN"

## ## This data is not provided in the public data set
## ## read data
## ## note that the HK comparison data was collected separately
## ## the small size of HK allowed us to scrape all areas
## data <- read.table(
##     "../data/ChinaHKInstagramData_part_fixed.tsv.gz",
##     stringsAsFactors=F, sep="\t", quote="\""
##     )
## data.names <- data[1,]
## data <- data[-1,]
## names(data) <- as.vector(t(data.names))

## ## Chinese grids only (dataset also includes partial scrape of Hong Kong)
## data <- subset(
##     data,
##     grid %in%
##     with(subset(r.pts.cn.hk.only, iso3 == sample.iso3), paste(long,
##     lat, sep="_"))
##     )

## ## evaluate grid overlap
## mtrs_degr_long <- function(long) { a=6378137.0 ; b=6356752.3142
##          e.sqr <- a^2/b^2 -1; long=long*2*pi/360
##                                    pi*a*cos(long)/( 180*(1-e.sqr*sin(long)^2)^(1/2) ) }

## mtrs_degr_lat <- function(lat) {
##     lat * 111100
## }

## data$point.km <- latlong2grid(
##     with(data, as.matrix(data.frame(as.numeric(longitude), as.numeric(latitude)), ncol=2))
##     )

## data$grid.km.upper <- latlong2grid(
##     matrix(t(data.frame(sapply(data$grid, function(x) lapply(strsplit(x, "_"), function(y) as.numeric(y)+0.015)))), ncol=2)
##     )

## data$grid.km.lower <- latlong2grid(
##     matrix(t(data.frame(sapply(data$grid, function(x) lapply(strsplit(x, "_"), function(y) as.numeric(y)-0.015)))), ncol=2)
##     )

## nrow(unique(subset(data, grid.km.upper$x >= point.km$x & grid.km.lower$x <= point.km$x & grid.km.upper$y >= point.km$y & grid.km.lower$y <= point.km$y)[,grep("tude|grid", names(data), value=T, invert=T)]))
## the above number was a basis for our estimate of the proportion of grids successfully sampled. it covered 4.4% of the Chinese population and 30% of all unique data collected. We attempted to sample 25% of the population.

## nrow(unique(data[,grep("tude|grid", names(data), value=T, invert=T)]))

## ## grid sampling overlap
## data <- unique(data[,grep("tude|grid", names(data), value=T,
## invert=T)])

## ## we then use this processed data for analyses
## write.table(
##     data,
##     file=paste("../data/",sample.iso3,"InstagramData_part_fixed_dedup.tsv",sep=""),
##     sep="\t",
##     row.names=F
##     )

data <-
    fread(paste("../data/",sample.iso3,"InstagramData.tsv",sep=""), colClasses="character", sep="\t")

data$Date <- as.Date(data$created_time)

usersbydate <- tapply(data$user_id, data$Date, function (x)
    length(unique(x)))
usersbydate <- usersbydate[2:(length(usersbydate)-1)]
## plot(as.Date(names(usersbydate)), usersbydate)

mean(usersbydate[as.Date(names(usersbydate))>as.Date("2014-09-28")])
mean(usersbydate[as.Date(names(usersbydate))<as.Date("2014-09-29")])

dates <- as.Date(names(usersbydate))
## draw a line on the graph
fitbjb <- smooth.spline(usersbydate[1:28] ~ as.numeric(dates)[1:28],
                        nknots=5)
fitbja <- smooth.spline(usersbydate[29:length(usersbydate)] ~
                                                as.numeric(dates)[29:length(usersbydate)],
                                                nknots=4)

pdf("../figs/InstagramPlot.pdf", height=5,
    width=10)
par(mar=c(5, 6, 2, 2))
plot(as.Date(names(usersbydate))[1:29], usersbydate[1:29], pch=16, col="purple2",
          ylim=c(0,8500), xlab="Date (2014)",
     ylab="Number of Unique Geo-located\nInstagram Users",
     bty="n",
     xaxt="n", yaxt="n",
     cex.lab=1.5, cex=1.25,
     xlim=c(as.Date(names(usersbydate))[c(1,length(usersbydate))])
     )
points(as.Date(names(usersbydate))[29:length(usersbydate)], usersbydate[29:length(usersbydate)], pch=16, col=rgb(0, 0, 1, 1), cex=1.25
     )
axis(
    side=2,
    at=c(0, 2000, 4000, 6000, 8000),
    cex.axis=1.5
    )
axis(
    side=1,
    at=c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")),
    labels=as.character(format(c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")), "%b %d")),
    cex.axis=1.5
    )
axis(
    side=1,
    at=c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")),
    labels=as.character(format(c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")), "%b %d")),
    cex.axis=1.5,
    tcl=0, col="white"
    )
segments(
    x0=dates[1],
    x1=dates[29],
    y0=mean(usersbydate[1:29]),
    y1=mean(usersbydate[1:29]),
    col="purple2",
    lwd=3)
segments(
    x0=dates[29],
    x1=dates[length(usersbydate)],
    y0=mean(usersbydate[1:29]),
    y1=mean(usersbydate[1:29]),
    col="black",
    lwd=3,
    lty=2)
segments(
    x0=dates[1],
    x1=dates[length(usersbydate)],
    y0=0,
    y1=0,
    col="black",
    lwd=2)
polygon(
    c(dates[29], dates[29], dates[length(usersbydate)], dates[length(usersbydate)]),
    c(mean(usersbydate[29:length(usersbydate)]), mean(usersbydate[1:29]), mean(usersbydate[1:29]), mean(usersbydate[29:length(usersbydate)])),
    col=rgb(1, 0, 0, 0.1),
    border=NA
    )
polygon(
    c(dates[29], dates[29], dates[length(usersbydate)], dates[length(usersbydate)]),
    c(0, mean(usersbydate[29:length(usersbydate)]), mean(usersbydate[29:length(usersbydate)]), 0),
    col=rgb(0, 0, 1, 0.1),
    border=NA
    )
segments(
    x0=dates[29],
    x1=dates[length(usersbydate)],
    y0=mean(usersbydate[29:length(usersbydate)]),
    y1=mean(usersbydate[29:length(usersbydate)]),
    col="blue",
    lwd=3)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(100,7700),
            lty=3, lwd=3)
text(as.Date("2014-09-29"), 8250, "Instagram \n Block", cex=1.5)
text(as.Date("2014-10-15"), 5500, "Decreased information\n(lose access to Instagram)", col="red", cex=1.25)
text(as.Date("2014-10-15"), 1500, "Increased information\n(gain access to all blocked websites)", col="blue", cex=1.25)
dev.off()

#### SUPPORTING INFORMATION plots

setwd("../data")
## this is the separately collected HK data
## hk <- read.csv("../data/HongKongInstagramCounts.csv")
## hk <- rbind(hk, read.csv("../data/HongKongInstagramCounts_extended.csv"))

## numusers.hk <- NULL
## dates.hk <- as.Date(names(bydate.hk))
## for(i in 1:length(dates.hk)){
##     sub <- hk[as.Date(hk$date)==dates.hk[i],]
##     users <- unlist(sapply(sub$users, function (x) str_split(x,
##                                                              ",")[[1]]))
##     numusers.hk[i] <- length(unique(users))
## }

## hk <- data.frame(
##     date=dates.hk,
##     numusers=numusers.hk
##     )

## write.csv(hk, file="../data/HongKongInstagramCounts.csv")

hk <- read.csv(file="../data/HongKongInstagramCounts.csv")


pdf("../figs/InstagramPlot_CN_v_HK.pdf", height=5,
    width=8)
par(mar=c(5, 6, 2, 2))

plot(as.Date(names(usersbydate))[1:29], usersbydate[1:29], pch=16, col="purple2",
          ylim=c(0,8500), xlab="Date (2014)",
     ylab="Number of Unique Geo-located\nInstagram Users",
     bty="n",
     xaxt="n", yaxt="n",
     cex.lab=1.5, cex=1.25,
     xlim=c(min(as.Date(hk$date)), max(as.Date(hk$date)))
     )
points(as.Date(names(usersbydate))[29:length(usersbydate)], usersbydate[29:length(usersbydate)], pch=16, col=rgb(0, 0, 1, 1), cex=1.25
     )
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(100,7700),
            lty=3, lwd=3)
axis(
    side=2,
    at=c(0, 2000, 4000, 6000, 8000),
    cex.axis=1.5
    )
axis(
    side=1,
    at=c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")),
    labels=as.character(format(c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")), "%b %d")),
    cex.axis=1.5
    )
axis(
    side=1,
    at=c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")),
    labels=as.character(format(c(dates[c(1, 15, 29, 45)], as.Date("2014-10-31")), "%b %d")),
    cex.axis=1.5,
    tcl=0, col="white"
    )
## lines(dates[1:28], predict(fitbjb, as.numeric(dates))$y[1:28],
##             col="red")
segments(
    x0=dates[1],
    x1=dates[29],
    y0=mean(usersbydate[1:29]),
    y1=mean(usersbydate[1:29]),
    col="purple2",
    lwd=3)
segments(
    x0=dates[29],
    x1=dates[length(usersbydate)],
    y0=mean(usersbydate[1:29]),
    y1=mean(usersbydate[1:29]),
    col="black",
    lwd=3,
    lty=2)
segments(
    x0=dates[1],
    x1=dates[length(usersbydate)],
    y0=0,
    y1=0,
    col="black",
    lwd=2)
polygon(
    c(dates[29], dates[29], dates[length(usersbydate)], dates[length(usersbydate)]),
    c(mean(usersbydate[29:length(usersbydate)]), mean(usersbydate[1:29]), mean(usersbydate[1:29]), mean(usersbydate[29:length(usersbydate)])),
    col=rgb(1, 0, 0, 0.1),
    border=NA
    )
polygon(
    c(dates[29], dates[29], dates[length(usersbydate)], dates[length(usersbydate)]),
    c(0, mean(usersbydate[29:length(usersbydate)]), mean(usersbydate[29:length(usersbydate)]), 0),
    col=rgb(0, 0, 1, 0.1),
    border=NA
    )
segments(
    x0=dates[29],
    x1=dates[length(usersbydate)],
    y0=mean(usersbydate[29:length(usersbydate)]),
    y1=mean(usersbydate[29:length(usersbydate)]),
    col="blue",
    lwd=3)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(100,7700),
            lty=3, lwd=3)
text(as.Date("2014-09-29"), 8250, "Instagram \n Block", cex=1.5)

points(as.Date(hk$date), hk$numusers, pch=16)
fithk <- smooth.spline(hk$numusers ~ as.numeric(hk$date), nknots=8)
lines(dates, predict(fithk, as.numeric(dates))$y, lwd=2)

text(as.Date("2014-10-15"), 5000, "Mainland China (~1 out of 4 Instagram posts in CN)", col="blue", cex=1.25)
text(as.Date("2014-10-15"), 1000, "Hong Kong (most to all Instagram posts in HK)", col="black", cex=1.25)

dev.off()


#Users who stay
## This is an analysis in the appendix analyzing activity levels and staying on Instagram
## data <- as.data.frame(data)

## max <- tapply(data$Date, data$user_id, max)
## stay <- names(max)[max>as.numeric(as.Date("2014-09-28"))]
## go <- names(max)[max<as.numeric(as.Date("2014-09-29"))]

## datab <- data[data$Date<as.Date("2014-09-29"),]
## likes <- tapply(as.numeric(datab$likes_count), datab$user_id, mean,
##                 na.rm=T)

## comments <- tapply(as.numeric(datab$comments_count), datab$user_id,
##                    mean, na.rm=T)

## save(likes, comments, stay, go, file="../data/instagram_likes_comments.RData")

load(file="../data/instagram_likes_comments.RData")


likesttest <- t.test(log(likes[names(likes)%in%stay]+1), log(likes[names(likes)%in%go]+1))

commentsttest <- t.test(log(comments[names(comments)%in%stay]+1), log(comments[names(comments)%in%go]+1))

pdf("../figs/InstagramStayGo.pdf", width=7, height=4)
plot(c(0,0), col="white", xlim=c(-.1,.6), ylim=c(.9,2.1), xlab=c("Mean Difference (Users Who Stay - Users Who Leave)"), yaxt="n", ylab="", bty="n", cex.lab=1.5, cex.axis=1.5)
lines(c(0,0), c(-10,10), lty=2, lwd=2)
points(c(likesttest$estimate[1]-likesttest$estimate[2]),1, pch=16, cex=1.25)
lines(c(likesttest$conf.int[1], likesttest$conf.int[2]), c(1,1), lwd=2)
points(c(commentsttest$estimate[1]-commentsttest$estimate[2]),2, pch=16, cex=1.25)
lines(c(commentsttest$conf.int[1], commentsttest$conf.int[2]), c(2,2), lwd=2)
text(.29,2, "Log Comments", cex=1.25)
text(.57,1, "Log Likes", cex=1.25)
dev.off()
