setwd("../data")
library(stringr)
## global geolocations subset to CN and HK
load("CN_2014-01-01_2015-12-31_short.RData")

data <- DATA
rm(list="DATA")

load("HK_2014-01-01_2015-12-31_short.RData")
hk <- DATA
rm(list="DATA")

## ids are assigned by file (same ID in CN/HK is not same person)


#Hong Kong/China Twitter diff-in-diff
## subset down to right around instagram block
subcn <- data[as.Date(data$date)<as.Date("2014-10-07") & as.Date(data$date)>as.Date("2014-09-19"),]
subhk <- hk[as.Date(hk$date)<as.Date("2014-10-07") & as.Date(hk$date)>as.Date("2014-09-19"),]

bydatecn <- tapply(subcn$user_id_str, subcn$date, function (x) length(unique(x)))
bydatehk <- tapply(subhk$user_id_str, subhk$date, function (x)
    length(unique(x)))

dates <- unique(as.Date(names(bydatehk)))
## draw lines
## bad fit, but just need highlights
fithk <- smooth.spline(bydatehk ~ as.numeric(dates), nknots=5)
fitbjb <- smooth.spline(bydatecn[1:9] ~ as.numeric(dates)[1:9],
                                                nknots=2)
fitbja <- smooth.spline(bydatecn[10:17] ~ as.numeric(dates)[10:17],
                        nknots=2)

twitter.col <- rgb(80, 154, 193, maxColorValue=255)
insta.col <- rgb(205, 61, 110, maxColorValue=255)

pdf("../figs/DiffandDiffTwitter.pdf")
plot(dates, bydatehk, pch=16, ylim=c(0,2500),
          ylab="Number of Unique Users",
          xlab="Date", bty="n", cex.axis=1.7, cex=1.4, cex.lab=1.7)
lines(c(as.Date("2014-09-29"),as.Date("2014-09-29")), c(0,100000),
      lty=2)
lines(dates, predict(fithk, as.numeric(dates))$y)
points(dates, bydatecn, pch=16, col=twitter.col, cex=1.4)
lines(dates[1:10], predict(fitbjb, as.numeric(dates))$y[1:10],
      col=twitter.col, lwd=2)
lines(dates[10:17], rep(mean(bydatecn[10:17]), 8),
      col=twitter.col, lwd=2)
##
text(as.Date("2014-09-24"), 2000, "China Twitter Users", col=twitter.col, cex=1.7)
text(as.Date("2014-09-24"), 500, "Hong Kong Twitter Users", cex=1.7)
dev.off()



## #VPN count all data
## library(stringr)
## vpn <- sapply(data$tweet_text, function (x) str_detect(x,"vpn") |
##                                              str_detect(x, "VPN"))
## vpn mentions

## fanqiang <- grepl("翻墙", data$tweet_text) | grepl('vpn', data$tweet_text)
## data$fanqiang <- fanqiang
## ## bydate is bydatevpn

## ## hong kong mentions
## hkmentions <- sapply(data$tweet_text, function (x) str_detect(x,"香港") |
##                  str_detect(x, "hk")| str_detect(x, "hong kong"))
## data$hk <- hkmentions

## ## instagram mentions
## ins <- sapply(data$tweet_text, function (x) str_detect(x,"ins"))
## data$ins <- ins

bydatehk <- tapply(data$hk, data$date, sum)
bydatehktotal <- tapply(data$hk, data$date, length)

bydate <- tapply(data$fanqiang, data$date, sum)
bydatetotal <- tapply(data$fanqiang, data$date, length)
usertotal <- tapply(data$user_id_str, data$date, function (x)
                    length(unique(x)))

bydateins <- tapply(data$ins, data$date, sum)


#week plot
bydatehksub <- bydatehk[as.Date(names(bydatehk))<as.Date("2014-10-07")
                        &
                            as.Date(names(bydatehk))>as.Date("2014-09-20")]
bydatehktotalsub <- bydatehktotal[as.Date(names(bydatehktotal))<as.Date("2014-10-07")
                        &
                            as.Date(names(bydatehktotal))>as.Date("2014-09-20")]

bydatevpnsub <- bydate[as.Date(names(bydate))<as.Date("2014-10-07")
                        &
                            as.Date(names(bydate))>as.Date("2014-09-20")]
usertotalsub <- usertotal[as.Date(names(usertotal))<as.Date("2014-10-07")
                        &
                            as.Date(names(usertotal))>as.Date("2014-09-20")]
bydateinssub <- bydateins[as.Date(names(bydateins))<as.Date("2014-10-07")
                        &
                            as.Date(names(bydateins))>as.Date("2014-09-20")]



pdf("../figs/UserEffects.pdf", height=7, width=7)
par(mar=c(9.1,6.1,7.1,2.1))
plot(as.Date(names(bydateinssub)), bydateinssub/bydatehktotalsub, col=insta.col, pch=16,
     ## ylim=c(0,3000),
     ylab="Proportion of Tweets that \n Mention `ins'",
     xlab="Date (2014)", cex.lab=1.2, bty="n", cex.lab=1.7, cex=1.4, cex.axis=1.7)
lines(c(as.Date("2014-09-26"), as.Date("2014-09-26")), c(-10,2000),
      lty=2)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(-10,2500),
      lty=2)
text(as.Date("2014-09-29"),2650, "Instagram is \n Blocked", cex=1.7)
text(as.Date("2014-09-26"),2150, "Hong Kong \n Protests Begin", cex=1.7)
lines(as.Date(names(bydateinssub)), bydateinssub/bydatehktotalsub,
      lty=3, col=insta.col)
mtext("Proportion of Tweets that \n Mention `ins'", 4, 4, at=900,
      cex=1.7)
dev.off()#




## SUPPORTING INFORMATION

## all of this is to show to what extent signups dropped and whether there was a sudden drop (possibly due to raising the wall)

## load("CN_2014-01-01_2015-12-31_short.RData")
## data <- DATA
## rm(list="DATA")

## load("HK_2014-01-01_2015-12-31_short.RData")
## hk <- DATA
## rm(list="DATA")

#@Create categories of users
## data$createdat <- as.Date(data$user_created_at,
##                               format="%a %b %d %H:%M:%S %z %Y")

## sign up dates on the 29th
## original code used screen name instead of user id
## replaceed screen name with user id str
totallynewusers <-
    unique(data$user_id_str[data$createdat==as.Date("2014-09-29")])

## people already on twitter
regularusers <-  unique(data$user_id_str[data$date<as.Date("2014-09-29")])

## people already on twitter and active on 29th
resumingusers <- unique(data$user_id_str[data$date==as.Date("2014-09-29")])
resumingusers <-
    resumingusers[!resumingusers%in%regularusers & !resumingusers%in%totallynewusers]

## people active on day before
placebo <-
    unique(data$user_id_str[data$createdat==as.Date("2014-09-28")])

## output <- list(totallynewusers=totallynewusers,
##                regularusers=regularusers, resumingusers=resumingusers, placebo=placebo)

## #save(output, file="UsersByCategory.RData")



## hk$createdat <- as.Date(hk$user_created_at,
##                               format="%a %b %d %H:%M:%S %z %Y")

totallynewusershk <-
    unique(hk$user_id_str[hk$createdat%in%c(as.Date("2014-09-29"), as.Date("2014-09-30"), as.Date("2014-10-01"),as.Date("2014-10-02"), as.Date("2014-10-03"), as.Date("2014-10-03"), as.Date("2014-10-04"))])

totallynewusers <-
    unique(data$user_id_str[data$createdat%in%c(as.Date("2014-09-29"), as.Date("2014-09-30"), as.Date("2014-10-01"),as.Date("2014-10-02"), as.Date("2014-10-03"), as.Date("2014-10-03"), as.Date("2014-10-04"))])

newusers <- data[data$user_id_str%in%totallynewusers,]
newusers$twitterday <-
    as.numeric(as.Date(newusers$date))-as.numeric(newusers$createdat)

hknewusers <- hk[hk$user_id_str%in%totallynewusershk,]
hknewusers$twitterday <-
    as.numeric(as.Date(hknewusers$date))-as.numeric(hknewusers$createdat)


#dropoff bydate
newusersbydate <- tapply(newusers$user_id_str, newusers$twitterday,function
                         (x) length(unique(x)))

#dropoffhk
newusersbydatehk <- tapply(hknewusers$user_id_str,
                           hknewusers$twitterday, function (x) length(unique(x)))


newusersbydate <-
    newusersbydate[!names(newusersbydate)%in%c("2014-12-02",
                                               "2014-12-05")]
newgusersbydatehk <- newusersbydatehk[!names(newusersbydatehk)%in%c("2014-12-02", "2014-12-05")]
dates <- as.numeric(names(newusersbydatehk))
fithk <- smooth.spline(newusersbydatehk/length(totallynewusershk) ~
                       as.numeric(dates), nknots=40)
dates <- as.numeric(names(newusersbydate))
fitbj <- smooth.spline(newusersbydate/length(totallynewusers) ~ as.numeric(dates), nknots=40)


#created by date
createdbydatehk <- tapply(hk$user_id_str, hk$createdat, function
                          (x) length(unique(x)))
createdbydate <- tapply(data$user_id_str, data$createdat, function
                          (x) length(unique(x)))


pdf("../figs/CreatedByDate.pdf", width=8, height=4)
par(mar=c(5, 6, 2, 2))
plot(as.Date(names(createdbydate)),
     log(createdbydate), pch=16, xlim=c(as.Date("2014-09-01"),
                                as.Date("2015-01-01")),
     xlab="Date (2014/2015)",
     ylab="Number of New Geolocated\nAccounts Created, By Day", col=twitter.col, bty="n",
     ylim=c(2.25, 7), yaxt="n", cex.lab=1.5, xaxt="n")
axis(
    side=2,
    at=log(c(10, 50, 250, 1000)),
    labels=c(10, 50, 250, 1000)
    )
axis(
    side=1,
    at=as.Date(c("2014-09-01","2014-09-29", "2014-11-01", "2014-12-01", "2015-01-01")),
    labels=format(as.Date(c("2014-09-01","2014-09-29", "2014-11-01", "2014-12-01", "2015-01-01")), "%b %d")
    )
points(as.Date(names(createdbydatehk)),
       log(createdbydatehk), pch=16)
lines(loess.smooth(as.Date(names(createdbydatehk))[2862:2905],
       log(createdbydatehk*2)[2862:2905], degree=2, span=1), lwd=2, col=twitter.col, lty=2)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(0,600),
      lty=2)
text(as.Date("2014-10-20"), log(300), "New Accounts, \n China Users", col=twitter.col)
text(as.Date("2014-10-20"), log(12), "New Accounts, \n Hong Kong Users")
text(as.Date("2014-12-20"), log(500), "original y-axis")

u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)
v <- c( (v[1]+v[2])/1.75, v[2], (v[3]+v[4])/1.75, v[4] )
par( fig=v, new=TRUE, mar=c(0,0,0,0) )
plot(as.Date(names(createdbydate)),
     (createdbydate), pch=16, xlim=c(as.Date("2014-09-01"),
                                as.Date("2015-01-01")),
     col=twitter.col, bty="n", xlab="", ylab="", xaxt="n", yaxt="n", cex=0.5)
axis(
    side=1,
    at=as.Date(c("2014-09-01","2014-09-29", "2014-11-01", "2014-12-01", "2015-01-01")),
    labels=FALSE)
Axis(side=2, labels=F)
dev.off()
