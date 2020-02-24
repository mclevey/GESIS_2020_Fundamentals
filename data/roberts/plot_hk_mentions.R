load(file="../data/FirstTweetsCodedByRA.RData")

## RA <- read.csv("FirstTweetsForRA.csv")
## RA <- RA[-c(1471:1475),] # huh
## RA$date <- sapply(RA$created_at, function (x) str_split(x, " ")[[1]][1])
## RA$date <- sapply(RA$date, function (x) gsub("/", "-", x))
## RA$date <- as.Date(RA$date, format="%m-%d-%Y")

RA <- RA[RA$date>as.Date("2014-09-28"),]
nothk <- RA[!RA$hk,]

bydatehk <- tapply(nothk$Talk.about..hong.kong. & nothk$Talk.about.politics., nothk$date, mean,na.rm=T)

## RAr <- read.csv("FirstTweetsForRA_regresume.csv")
## RAr <-RAr[-c(3761,3762),]
## RAr$date <- sapply(RAr$created_at, function (x) str_split(x, " ")[[1]][1])
## RAr$date <- sapply(RAr$date, function (x) gsub("/", "-", x))
## RAr$date <- as.Date(RAr$date, format="%m-%d-%Y")
#RAr <- RAr[RAr$date>as.Date("2014-09-28") & RAr$date<as.Date("2014-10-04"),]
nothkr <- RAr[!RAr$hk,]

bydatehkr <- tapply(nothkr$Talk.about..hong.kong. & nothkr$Talk.about.politics., nothkr$date, mean, na.rm=T)

umbrella.col <- rgb(240, 220, 110, maxColorValue=255)

pdf("..//figs/plot_hk_mentions.pdf",width=4,height=4)
par(mar=c(5.1, 5.1, 4.1, 1.1))

plot(as.Date(names(bydatehk)), bydatehkr, ylim=c(0,.06), pch=16,
     xlab="Date (2014)",
     ylab="Percentage of Tweets\nDiscussing Politics and Hong Kong",
     xlim=c(as.Date("2014-09-29"), as.Date("2014-10-06")), bty="n")
lines(as.Date(names(bydatehk)), bydatehkr, lty=2)
points(as.Date(names(bydatehkr)), bydatehk, pch=16, col="darkorange")
lines(as.Date(names(bydatehkr)), bydatehk, lty=2, col="darkorange")
text(as.Date("2014-09-30")-.3, .05, "Old \n Users")
text(as.Date("2014-09-30")-.3, .01, "New \n Users", col="darkorange")
dev.off()
