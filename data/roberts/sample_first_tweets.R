#Please contact authors about data (cannot be deidentified)
library(stringr)
library(reshape)
library(ggplot2)

load(file="../data/HK_2014-01-01_2015-12-31.RData")
HK <- DATA
HK <- subset(HK, country_code=="HK")$user_id_str

load("../data/CN_2014-01-01_2015-12-31_short.RData")
data <- DATA

load("../data/instagram_oneweek_sample_parsed_short.RData")

tweets$created_at <- sub("\"","", tweets$created_at)
tweets$created_at_date <-  as.POSIXlt(tweets$created_at, format="%a %b %d %H:%M:%S %z %Y",tz="GMT")

data$createdat <- as.Date(data$user_created_at,
                              format="%a %b %d %H:%M:%S %z %Y")
totallynewusers <-
    unique(data$user_id_str[data$createdat==as.Date("2014-09-29")])

regularusers <-  unique(data$user_id_str[data$date<as.Date("2014-09-29")])

resumingusers <- unique(data$user_id_str[data$date==as.Date("2014-09-29")])
resumingusers <-
    resumingusers[!resumingusers%in%totallynewusers]

regularresumingusers <- unique(data$user_id_str[data$date==as.Date("2014-09-29")&data$createdat<as.Date("2014-09-29")])

## add users who tweeted on the 29th
tweets$user.id <- gsub("\"","",tweets$user.id)
regularresumingusers.notgeo <- unique(tweets[as.Date(tweets$created_at_date) == as.Date("2014-09-29") & tweets$user.id %in% regularusers & tweets$user.id %in% unique(data$user_id_str),]$user.id)

regularresumingusers <- unique(c(regularresumingusers, regularresumingusers.notgeo))

## mean(regularresumingusers.check %in% regularresumingusers)


placebo <-
    unique(data$user_id_str[data$createdat==as.Date("2014-09-28")])

placebo2 <-
    unique(data$user_id_str[data$createdat>=as.Date("2014-09-22") & data$createdat<=as.Date("2014-09-28")])

save(placebo2, file="../data/twitter_instagram_placebo2_users.RData")

output <- list(totallynewusers=totallynewusers,
               regularusers=regularusers, resumingusers=resumingusers, placebo=placebo)


setwd("../data/")

tweets$created_at <- sub("\"","", tweets$created_at)

tweets$user.id <- gsub("\"", "", tweets$user.id)

tweets$in.hk <- tweets$user.id %in% HK

tweets$created_at_date <-  as.POSIXlt(tweets$created_at, format="%a %b %d %H:%M:%S %z %Y",tz="GMT")

the.type <- "regularresuming"
the.list <- regularresumingusers

first.tweets <- subset(tweets, user.id %in% the.list & as.Date(created_at_date) >= as.Date("2014-09-29"))
first.tweets <- unique(first.tweets)

first.tweets.ordered <- first.tweets[order(first.tweets$created_at_date), ]
d <- by(first.tweets.ordered, first.tweets.ordered["user.id"], head, n=10)
first.tweets.top10 <- Reduce(rbind, d)

save(first.tweets.top10, file=paste("../data/first_tweets_",the.type,".RData",sep=""))

data$in.hk <- data$user_id_str %in% HK

first.tweets.geo <- subset(data, user_id_str %in% the.list & date >= as.Date("2014-09-29") & date <= as.Date("2014-09-29") + 14)
first.tweets.geo <- unique(first.tweets.geo)

first.tweets.geo$created_at_date <- as.POSIXlt(first.tweets.geo$tweet_created_at, format="%a %b %d %H:%M:%S %z %Y", tz="GMT")

first.tweets.geo.ordered <- first.tweets.geo[order(first.tweets.geo$created_at), ]
d <- by(first.tweets.geo.ordered, first.tweets.geo.ordered["user_id_str"], head, n=10)
first.tweets.geo.top10 <- Reduce(rbind, d)

save(first.tweets.geo.top10, file=paste("../data/first_tweets_geo_",the.type,".RData",sep=""))

first.tweets.geo.short <- with(first.tweets.geo, data.frame(
    user.id = user_id_str,
    text = tweet_text,
    created_at = created_at_date
    ))

first.tweets.short <- with(first.tweets, data.frame(
    user.id = user.id,
    text = text,
    created_at = created_at_date
    ))

first.tweets.all <- rbind(first.tweets.geo.short, first.tweets.short)

first.tweets.all <- unique(first.tweets.all)

first.tweets.all$geo <- with(first.tweets.all, paste(text, created_at, user.id)) %in% with(first.tweets.geo, paste(tweet_text, created_at_date, user_id_str))

first.tweets.all$hk <- first.tweets.all$user.id %in% HK

first.tweets.all$text <- gsub("\"", "", first.tweets.all$text)
first.tweets.all <- unique(first.tweets.all)

first.tweets.all.ordered <- first.tweets.all[order(first.tweets.all$created_at), ]
d <- by(first.tweets.all.ordered, first.tweets.all.ordered["user.id"], head, n=10)
first.tweets.all.top10 <- Reduce(rbind, d)

save(first.tweets.all.top10, file=paste("../data/first_tweets_all_",the.type,".RData",sep=""))


setwd("../data")
load("first_tweets_all_new.RData")

n.users.new <- length(unique(first.tweets.all.top10$user.id))

the.type <- "regularresuming"

load(paste("first_tweets_all_",the.type,".RData",sep=""))

first.tweets.all.top10$text<- sapply(first.tweets.all.top10$text, function (x) gsub('"', '', x))
first.tweets.all.top10 <- first.tweets.all.top10[!duplicated(cbind(first.tweets.all.top10$text, first.tweets.all.top10$user.id)),]


if (the.type=="regular" | the.type=="regularresuming") {
the.users <- unique(first.tweets.all.top10$user.id)

set.seed(34521)
the.users.sample <- sample(the.users, n.users.new)

first.tweets.top10.sample <- subset(first.tweets.all.top10, user.id %in% the.users.sample)
} else {
    first.tweets.top10.sample <- first.tweets.all.top10
}

write.csv(first.tweets.top10.sample, paste("FirstTweetsForRA_",the.type,ifelse(the.type=="regular" | the.type=="regularresuming","_500",""),".csv",sep=""), row.names=F)
