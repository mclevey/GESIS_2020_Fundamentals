rm(list = ls())

library(plyr)

## this code plots added followers
## the follower sign ups are from a 10% random sample of followers
## load("../data/follower_counts_by_date.RData")

## all.counts <- list()

## for (which.account in grep("counts.table", ls(), value=T, fixed=T)) {

## counts.df <- data.frame(names(get(which.account)[names(get(which.account)) %in% as.character(seq(as.Date("2014-09-01"), as.Date("2014-11-01"), by="day"))]), as.vector(get(which.account)[names(get(which.account)) %in% as.character(seq(as.Date("2014-09-01"), as.Date("2014-11-01"), by="day"))]))
## names(counts.df) <- c("date","count")

## counts.df <- merge(counts.df, data.frame(date=as.character(seq(as.Date("2014-09-01"), as.Date("2014-11-01"), by="day")), present=rep(1, 62)), by="date", all.y=T)
## counts.df$count[is.na(counts.df$count)] <- 0
## counts.df$from.block <- as.Date(counts.df$date) - as.Date("2014-09-28")

## all.counts[[which.account]] <- counts.df

## }


## all.counts.df <- ldply(all.counts, data.frame)
## all.counts.df <- all.counts.df[,c("date","count","present","from.block")]

## save(counts.table.nytchinese, counts.table.appledaily_hk, all.counts.df, file="../data/follower_counts_by_date_replication.RData")

load(file="../data/follower_counts_by_date_replication.RData")

all.counts.agg <- aggregate(count ~ date, all.counts.df, FUN = sum)
all.counts.agg <- all.counts.agg[order(as.Date(all.counts.agg$date)),]


twitter.col <- rgb(80, 154, 193, maxColorValue=255)

pdf("../figs/spike_increase_instagram_on_twitter_cumulative.pdf", width=8, height=4)
par(mar=c(5,6,3,5))
plot(as.Date(all.counts.agg[order(as.Date(all.counts.agg$date)),]$date), cumsum(all.counts.agg[order(as.Date(all.counts.agg$date)),]$count * 10 - mean(subset(all.counts.agg[order(as.Date(all.counts.agg$date)),], as.character(date) < "2014-09-29")$count*10)), pch=16, col=twitter.col, bty="n", ylab="Extra Followers (cumulative)", xlab="Follower Sign-up Date", cex.lab=1.5, lwd=3, type="s", cex.axis=1.25, xaxt="n")
axis(
    side=1,
    at=as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15", "2014-11-01")),
    labels=as.character(format(as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15", "2014-11-01")), "%b %d")),
    cex.axis=1.5
    )
abline(v=as.Date("2014-09-29"), col="red", lty=2, lwd=2)
abline(h=0, lty=3, lwd=2)
mtext("Instagram block", 3, at=as.Date("2014-09-29"), cex=1.25, col="red")

total.effect <- rev(cumsum(all.counts.agg[order(as.Date(all.counts.agg$date)),]$count * 10 - mean(subset(all.counts.agg[order(as.Date(all.counts.agg$date)),], as.character(date) < "2014-09-29")$count*10)))[1]
segments(x0=as.Date("2014-11-01")+2, x1=as.Date("2014-11-01")+2, y0=0, y=total.effect, col="red", lwd=2)
segments(x0=as.Date("2014-11-01")+2, x1=as.Date("2014-11-01")+2, y0=0, y=total.effect, col="red", lwd=2)
segments(x0=as.Date("2014-11-01")+1, x1=as.Date("2014-11-01")+2, y0=total.effect, y=total.effect, col="red", lwd=2)
segments(x0=as.Date("2014-11-01")+1, x1=as.Date("2014-11-01")+2, y0=0, y=0, col="red", lwd=2)
mtext("Increase\nin follow\nrelationships:", side=4, at=23000, col="red", las=1, cex=0.9)
mtext(prettyNum(total.effect, big.mark=","), side=4, col="red", las=1, adj=-0.05)
text(x=as.Date("2014-10-16"),y=1200,labels="expected number of follow relationships", cex=0.9)

text(as.Date("2014-09-15"), 20000, "Cumulative increase in follow\nrelationships compared to\npre-block trend\n(all Chinese language Twitter)")

dev.off()


pdf("../figs/spike_increase_instagram_on_twitter_marginal_examples.pdf", width=6, height=6)
par(mfrow=c(2, 1))
counts.table.nytchinese.short <- counts.table.nytchinese[names(counts.table.nytchinese) %in% as.character(seq(as.Date("2014-09-01"), as.Date("2014-11-01"), by="day"))]

plot(as.Date(names(counts.table.nytchinese.short)), as.vector(counts.table.nytchinese.short), type="l", bty="n", xaxt="n", yaxt="n", lwd=2, main="NYT Chinese", cex.main=0.8, xlab="Twitter Sign-up Date", ylab="", cex.main=1.5, cex.lab=1.5)
title(ylab="Number of followers", line=2, cex.lab=1.5)
abline(v=as.Date("2014-09-29"), lty=2, lwd=3, col="red")
axis(side=1, at=as.Date(c("2014-09-01", "2014-09-29", "2014-11-01")), labels=format(as.Date(c("2014-09-01", "2014-09-29", "2014-11-01")), "%b %d"), cex.axis=0.8, cex.lab=0.8, padj=-0.5, cex.axis=1.2)
mtext(c(25, 100), at=c(25, 100), side=2, las=1, cex=1.3)
mtext(c(40), at=c(40), side=4, las=1, cex=1.3)


counts.table.appledaily_hk.short <- counts.table.appledaily_hk[names(counts.table.appledaily_hk) %in% as.character(seq(as.Date("2014-09-01"), as.Date("2014-11-01"), by="day"))]

plot(as.Date(names(counts.table.appledaily_hk.short)), as.vector(counts.table.appledaily_hk.short), type="l", bty="n", xaxt="n", yaxt="n", lwd=2, main="Apple Daily", cex.main=0.8, xlab="Twitter Sign-up Date", ylab="", cex.main=1.5, cex.lab=1.5)
title(ylab="Number of followers", line=2, cex.lab=1.5)
abline(v=as.Date("2014-09-29"), lty=2, lwd=3, col="red")
axis(side=1, at=as.Date(c("2014-09-01", "2014-09-29", "2014-11-01")), labels=format(as.Date(c("2014-09-01", "2014-09-29", "2014-11-01")), "%b %d"), cex.axis=0.8, cex.lab=0.8, padj=-0.5, cex.axis=1.2)
mtext(c(3, 38), at=c(3, 38), side=2, las=1, cex=1.3)
mtext(c(8), at=c(8), side=4, las=1, cex=1.3)
## text(x=as.Date("2014-09-15"), y=75, labels="# of followers,\nby sign-up date", cex=0.6)

dev.off()
