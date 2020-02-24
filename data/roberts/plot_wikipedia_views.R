library(xtable)

## this script plots Wikipedia page view data, given that a page was blocked in china on a given day

load("../data/all_zh_blocked_page_counts.RData")

data <- BLOCKED

data$time <- as.POSIXct(data$time, gz="UTC")
data$time <- format(data$time, tz="Hongkong")

#pages with largest changes
aggbypage <- aggregate(count_views ~ as.Date(time) + page_title, data = subset(data, page_title!="%E4%B8%AD%E5%8D%8E%E4%BA%BA%E6%B0%91%E5%85%B1%E5%92%8C%E5%9B%BD"), FUN = sum)
names(aggbypage) <- c("date", "page_title", "count_views")

datasub <- aggbypage[as.Date(aggbypage$date)%in%c(as.Date("2014-09-28"), as.Date("2014-09-29")),]
datasub <- datasub[order(datasub$date),]
diffs <- tapply(datasub$count_views, datasub$page_title, function (x) x[2]-x[1])
cat(paste(sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode), collapse="\n"))

datasub <- aggbypage[as.Date(aggbypage$date)%in%c(as.Date("2014-09-27"), as.Date("2014-09-28")),]
datasub <- datasub[order(datasub$date),]
diffs <- tapply(datasub$count_views, datasub$page_title, function (x) x[2]-x[1])
cat(paste(sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode), collapse="\n"))


datasub <- aggbypage[as.Date(aggbypage$date)%in%c(as.Date("2014-09-29"), as.Date("2014-09-30")),]
datasub <- datasub[order(datasub$date),]
diffs <- tapply(datasub$count_views, datasub$page_title, function (x) x[2]-x[1])
cat(paste(sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode), collapse="\n"))


datasub <- aggbypage[as.Date(aggbypage$date)%in%c(as.Date("2014-09-28"),as.Date("2014-09-29"), as.Date("2014-09-30")),]
datasub$before <- ifelse(as.Date(datasub$date)<as.Date("2014-09-29"),1,0)
datasub <- datasub[order(datasub$before),]
diffs <- tapply(datasub$count_views, datasub$page_title, function (x) x[2]-x[1])
cat(paste(sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode), collapse="\n"))
mat <- matrix(NA, nrow=20, ncol=2)
mat[,1] <-  paste("cntext{",sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode),"}", sep="")
mat[,2] <- c("blacklisted words", "Jiang Zemin", "Radio Australia", "Hu Jintao",
             "Zeng Qinghong", "Tank Man", "Li Peng", "Corruption",
             "Tiananmen Incident", "Zhou Yongkang", "Wu'erkaixi",
             "Zhang Dejiang", "Youtube", "Wen Jiabao",
             "Japanese pornstars", "Mao Zedong", "Deng Xiaoping", "Ling Gu", "Wu Bangguo",
             "Hua Guofeng")
colnames(mat) <- c("Wikipedia Page", "Translation")
xtable(mat)

datasub <- aggbypage[as.Date(aggbypage$date)%in%c(as.Date("2014-09-28"), as.Date("2014-10-23")),]
datasub <- datasub[order(datasub$date),]
diffs <- tapply(datasub$count_views, datasub$page_title, function (x) x[2]-x[1])
cat(paste(sapply(names(sort(diffs, decreasing=T)[1:20]), URLdecode), collapse="\n"))

## check for traditional
## cat(paste(sapply(head(grep("%E4%BA%8B%E4%BB%B6", grep("%E5%A4%A9%E5%AE%89", names(sort(diffs, decreasing=T)), value=T), value=T), n=20), URLdecode), collapse="\n"))

agg <- aggregate(count_views ~ as.Date(time), data = subset(data, page_title!="%E4%B8%AD%E5%8D%8E%E4%BA%BA%E6%B0%91%E5%85%B1%E5%92%8C%E5%9B%BD"), FUN = sum)
names(agg) <- c("date", "count_views")

wiki.color <- rgb(60, 60, 60, maxColorValue=255)

pdf("../figs/blocked_zh_wikipedia_page_views_r_and_r.pdf", width=6,height=3)
par(mar=c(5,6,2,2))
with(subset(agg, date >= as.Date("2014-09-01") & date <= as.Date("2014-10-31")), plot(date, count_views, type='l', xlim=as.Date(c("2014-09-01", "2014-10-31")), bty="n", xlab="Date", ylab="Number of views\n(Chinese language blocked pages)", xaxt="n", ylim=c(50000, 310000), cex.lab=0.95, yaxt="n", col=wiki.color, lwd=2))
text(as.numeric(as.Date("2014-09-15")), y=250000, labels="Wikipedia page views:\n\n", col=wiki.color, cex=0.9, font=4)
text(as.numeric(as.Date("2014-09-15")), y=250000, labels="\nblocked Chinese\nlanguage pages", col=wiki.color, cex=0.9)
axis(
    side=1,
    at=as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15","2014-10-31")),
    labels=as.character(format(as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15", "2014-10-31")), "%b %d"))
    )
axis(
    side=2,
    at=c(120000, 200000, 280000),
    labels=c("120k", "200k", "280k"),
    cex=0.8
    )
axis(
    side=2,
    at=c(200000),
    labels=c("200k"),
    cex.axis=0.8
    )
abline(v=as.Date("2014-09-29"), lty=2, lwd=2, col="red")
mtext("Instagram block", side=3, col="red")
dev.off()


agg <- aggregate(count_views ~ time, data = subset(data, page_title=="%E4%B8%AD%E5%8D%8E%E4%BA%BA%E6%B0%91%E5%85%B1%E5%92%8C%E5%9B%BD"), FUN = sum)
names(agg) <- c("date", "count_views")

## this was a huge outlier
pdf("../figs/blocked_zh_wikipedia_china_country_page_views_r_and_r.pdf", width=6,height=3)
par(mar=c(5,5,2,2))
with(agg, plot(as.numeric(as.POSIXlt(date)), count_views, type='l', bty="n", xlab="Hour", ylab="Number of views\n(China country page only)", xaxt="n", col=wiki.color))
text(as.numeric(as.POSIXlt("2014-10-15")), y=40000, labels="China country page outlier\n(country page not included in main result)", col=wiki.color, cex=0.85)
axis(
    side=1,
    at=as.numeric(as.POSIXlt(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15","2014-10-31"))),
    labels=as.character(format(as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15", "2014-10-31")), "%b %d"))
    )
dev.off()
