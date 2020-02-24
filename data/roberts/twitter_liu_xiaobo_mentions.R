library(dplyr)
library(ggplot2)

## ## user info from twitter api
## signups.liu <- read.table("../data/liu_xiaobo_lang_sample_sample_user_info.txt", sep="\t",header=T,fill=T)

## signups.liu$weights <- rep(1, length(signups.liu$statuses_count))
## signups.liu$date <- as.Date(as.POSIXct(signups.liu$created_at, format="%a %b %d %H:%M:%S +0000 %Y") + 0 * 60 * 60)

## signups.liu <- aggregate(weights ~ date + id_str, data = signups.liu, FUN = sum)
## signups.liu$counts <- rep(1, length(signups.liu$weights))

## agg.liu <- aggregate(counts ~ date, signups.liu, FUN = sum)

## save(agg.liu, file="../data/liu_xiaobo_lang_sample_sample_user_info.RData")

load(file="../data/liu_xiaobo_lang_sample_sample_user_info.RData")

agg <- agg.liu
agg[is.na(agg)] <- 0

agg2 <- agg %>% group_by(date) %>% summarise(counts=sum(counts))

agg$weeks <- cut(agg[,"date"]+1, breaks="week")
agg$months <- cut(agg[,"date"]+2, breaks="month")

agg.weeks <- agg %>% group_by(weeks) %>% summarise(counts=sum(counts))

pdf("../figs/insta_liu_xiaobo.pdf", width=7, height=3.5)

g <- ggplot(subset(agg.weeks, as.Date(weeks) >= "2014-08-01" & as.Date(weeks) < "2014-12-01"), aes(as.Date(weeks), counts)) + geom_point() + scale_x_date() +
    ylab("Aggregated by Week") + xlab("Week") + geom_line() +
    geom_vline(xintercept=as.numeric(as.Date("2014-09-29")), color="red") +
    guides(color=guide_legend(title="Beijing/Urumqi time zone"))  +
    annotate("text", x = as.Date("2014-09-24"), y = 38, label = "3.1x", col="red", size=5) +
    labs(title="Signup dates of people who mentioned Liu Xiabo after his death in July 2017") + theme_classic()

print(g)

dev.off()
