## library(rgdal)

library(devtools)
library(instaR)

out.dir <- "./china/"
top.dir <- "./"

load("../data/latitude_longitude_population_agg_china_hk_only.RData")

load("insta_token2")

#Search over China
#Latitude: 1 deg = 111.2 km
#Longitude: 1 deg = 69.1703234283616*cos(.0174532925199433*latitude) miles

latby <- 1/111.2*2.5
#farthest north point beijing 41.06
#farthest south point bejing 39.44
#farthest north point in china 53.55 North
#farthest southern point in china 20.23 North
## latit <- seq(39.44, 41.06, by=latby)

#farthest west point beijing 115.45
#farthest east point beijing 117.51
#farthest east point in china 134.75
#farthest west point in china 73.81
## longby <- 1/69.1703234283616*cos(.0174532925199433*53.55)*1.60934*2.5
## longit <- seq(115.45, 117.51, by=longby)
dates <- seq(as.Date("2014-09-01"), as.Date("2014-10-31"), by="day")

set.seed(1234)
## sampn <- length(longit)*length(latit)
## longlatmat <- matrix(sample(1:sampn, sampn),nrow=length(longit),
##                      ncol=length(latit))
## rownames(longlatmat) <- longit
## colnames(longlatmat) <- latit

sampn <- sample(1:nrow(r.pts.cn.hk.only), size=round(nrow(r.pts.cn.hk.only) * 0.01), prob=r.pts.cn.hk.only$pop/sum(r.pts.cn.hk.only$pop))
## note: we only scraped 5% of the population grid. we think 25% is a conservative estimate (corresponding to the initial * 0.02 goal) -- we also checked many locations that did not return posts either due to no posts or a scrape failure

mySearchInstagram <- function (tag = NULL, token, n = 100, lat = NULL, lng = NULL,
    distance = NULL, folder = NULL, mindate = NULL, maxdate = NULL,
    verbose = TRUE, sleep = 0)
{
    if (!is.null(tag))
        url <- paste0("https://api.instagram.com/v1/tags/", tag,
            "/media/recent?")
    if (!is.null(lat) && !is.null(lng)) {
        url <- paste0("https://api.instagram.com/v1/media/search?lat=",
            lat, "&lng=", lng)
        if (!is.null(distance))
            url <- paste0(url, "&distance=", distance)
        url <- paste0(url, "&count=", min(c(n, 100)))
    }
    if (!is.null(mindate))
        url <- paste0(url, "&min_timestamp=", as.numeric(as.POSIXct(mindate)))
    if (!is.null(maxdate))
        url <- paste0(url, "&max_timestamp=", as.numeric(as.POSIXct(maxdate)))
    if (!is.null(tag) & !is.null(mindate)) {
        message("\"mindate\" and \"maxdate\" options only work in combination with a location and will be ignored.")
    }
    if (!is.null(mindate) && as.POSIXct(mindate) > as.POSIXct(maxdate)) {
        stop("\"mindate\" should be less than \"maxdate\".")
    }
    content <- instaR:::callAPI(url, token)
    l <- length(content$data)
    if (l == 0)
        stop("0 posts found.")
    if (verbose)
        message(l, " posts")
    error <- 0
    while (is.null(content$meta) | content$meta != 200) {
        message("Error!")
        Sys.sleep(0.5)
        error <- error + 1
        content <- instaR:::callAPI(url, token)
        if (error == 3) {
            stop("Error")
        }
    }
    if (length(content$data) == 0) {
        stop("No public posts mentioning the string were found")
    }
    df <- instaR:::searchListToDF(content$data)
    if (!is.null(folder)) {
        if (verbose)
            message("Downloading pictures...")
        dir.create(file.path(getwd(), folder), showWarnings = FALSE)
        downloadPictures(df, folder)
    }
    if (sleep != 0) {
        Sys.sleep(sleep)
    }
    if (n > 20) {
        df.list <- list(df)
        if (length(content$pagination) > 0)
            next_url <- content$pagination["next_url"]
        if (length(content$pagination) == 0 & is.null(mindate)) {
            next_url <- paste0(url, "&max_timestamp=", as.numeric(min(df$created_time)))
        }
        if (length(content$pagination) == 0 & !is.null(mindate)) {
            next_url <- gsub("max_timestamp=([0-9]{10})", paste0("max_timestamp=",
                as.numeric(min(df$created_time))), url)
        }
        while (l < n & length(content$data) > 0 & !is.null(next_url[[1]])) {
            content <- instaR:::callAPI(next_url, token)
            error <- 0
            while (is.null(content$meta) | content$meta != 200) {
                message("Error!")
                Sys.sleep(0.5)
                error <- error + 1
                content <- instaR:::callAPI(url, token)
                if (error == 3) {
                  stop("Error")
                }
            }
            new.df <- instaR:::searchListToDF(content$data)
            if (all(new.df$id %in% unlist(lapply(df.list, "[[",
                "id")) == TRUE)) {
                break
            }
            l <- l + length(content$data)
            if (length(content$data) > 0) {
                message(l, " posts")
            }
            if (!is.null(folder)) {
                if (verbose)
                  message("Downloading pictures...")
                downloadPictures(new.df, folder)
            }
            df.list <- c(df.list, list(new.df))
            if (length(content$pagination) > 0)
                next_url <- content$pagination["next_url"]
            if (length(content$pagination) == 0 & is.null(mindate)) {
                next_url <- paste0(url, "&max_timestamp=", as.numeric(min(new.df$created_time)))
            }
            if (length(content$pagination) == 0 & !is.null(mindate)) {
                next_url <- gsub("max_timestamp=([0-9]{10})",
                  paste0("max_timestamp=", as.numeric(min(new.df$created_time))),
                  url)
            }
            if (sleep != 0) {
                Sys.sleep(sleep)
            }
        }
        df <- do.call(rbind, df.list)
    }
    return(df)
}

## run 1
# i: 1500780
## k: 27
##
## run 2
## i: 1133880
## k: 34
##
## run 3
## i: 13720256 (121.4125_31.2125)
## k: 49 (2014-10-19)
##
## run 4
## i: 1132218 121.4125_31.2125_2014-10-21
## k: 34
## run 5
## i: 121.5125_31.2625_2014-10-04
## which(with(r.pts.cn.hk.only, long == "121.5125" & lat == "31.2625"))
## k: 1
##
## run 6
## 3000 meters sample started here
## 121.4875_31.2125_2014-09-22
## which(with(r.pts.cn.hk.only, long == "121.4875" & lat == "31.2125"))
## which(dates=="2014-09-22")
##
## run 7
## 4000 meters sample started here
## 114.1875_35.8625
## 114.1875_35.8625_2014-09-08
for(i in sampn[which(sampn==821275):length(sampn)]){
        for(k in ifelse(i==821275, 10, 1):(length(dates)-1)){  #ifelse(i==1132218, 34, 1)
            ## entry <- which(longlatmat==i, arr.ind=T)
            ## long <- as.numeric(rownames(longlatmat)[entry[1]])
            ## lat <- as.numeric(colnames(longlatmat)[entry[2]])
            long <- r.pts.cn.hk.only[i,]$long
            lat <- r.pts.cn.hk.only[i,]$lat
            setwd(out.dir)
            folder=paste(long, lat,  dates[k], sep="_")
            completed.files <- list.files()
            completed.files <- gsub(".zip", "", completed.files, fixed=T)
            if (folder %in% completed.files) next
            the.errors <- read.table("errors.txt", stringsAsFactors=F,header=T)
            if (folder %in% the.errors[,1]) next
            tsq <- tryCatch({out <- searchInstagram(lat=lat, lng=long, distance=4000,
                                                    ## samples outside grid. evaluate later
                           token=token, n=1000, folder=folder, sleep=5,
                       mindate=dates[k], maxdate=dates[k+1])
            save(out, file=paste(out.dir, folder, "/out.RData", sep=""))
            system(paste("zip -r ", folder, ".zip ", folder,
                         sep=""))
            system(paste("rm -r ", folder,
                         sep=""))
                         }, error=function(error){
                             print(paste("MY_ERROR: ", folder, ":", error))
                             write.table(
                                 data.frame(folder=folder, error=as.character(error)),
                                 file="errors.txt",append=T,row.names=F,col.names=F,sep="\t"
                             )
                         }
                            )
          }
    }
#}

setwd(out.dir)
files <- grep("zip", list.files(), value=T)
i <- 1
    system(paste("unzip", files[i]))
## unzip(files[i])
folder <- strsplit(files[i], ".zip")[[1]][1]
split <- strsplit(folder, "_")[[1]]
long <- split[1]
lat <- split[2]
date <- split[3]
setwd(paste(out.dir, folder,
            sep=""))
files2 <- list.files()
users <- sapply(files2[!files2%in%"out.RData"], function (x) strsplit(x, "_")[[1]][3])
data <- data.frame(long=long, lat=lat, date=date,
                   num=length(files2[!files2%in%"out.RData"]),
                   users=paste(unique(users), collapse=","))
setwd(out.dir)
system(paste("rm -r ", folder,
             sep=""))

for(i in 2:length(files)){
    system(paste("unzip", files[i]))
    ## unzip(files[i])
    folder <- strsplit(files[i], ".zip")[[1]][1]
    split <- strsplit(folder, "_")[[1]]
    long <- split[1]
    lat <- split[2]
    date <- split[3]
    setwd(paste(out.dir, folder,
                sep=""))
    files2 <- list.files()
    users <- sapply(files2[!files2%in%"out.RData"], function (x) strsplit(x, "_")[[1]][3])
    data <- rbind(data, data.frame(long=long, lat=lat, date=date,
                       num=length(files2[!files2%in%"out.RData"]),
                             users=paste(unique(users), collapse=",")))
    setwd(out.dir)
    system(paste("rm -r ", folder,
                 sep=""))
    if(i%%100==0) print(i)
}

write.csv(data,paste(top.dir, "ChinaInstagramCounts.csv",sep=""), row.names=F)


setwd(out.dir)
files <- grep("zip", list.files(), value=T)
i <- 1
    system(paste("unzip", files[i]))
## unzip(files[i])
folder <- strsplit(files[i], ".zip")[[1]][1]
## split <- strsplit(folder, "_")[[1]]
## long <- split[1]
## lat <- split[2]
## date <- split[3]
setwd(paste(out.dir, folder,
            sep=""))
load("out.RData")
data2 <- out
data2 <- as.data.frame(sapply(data2, function(x) gsub("\"|'|\t", "", x)))
data2$grid <- rep(paste(strsplit(folder, "_")[[1]][1:2], collapse="_"), nrow(data2))
    write.table(data2,paste(top.dir, "ChinaHKInstagramData_part_fixed.tsv",sep=""), row.names=F, sep="\t")
setwd(out.dir)
system(paste("rm -r ", folder,
             sep=""))

for(i in 2:length(files)){
    system(paste("unzip", files[i]))
    ## unzip(files[i])
    folder <- strsplit(files[i], ".zip")[[1]][1]
    ## split <- strsplit(folder, "_")[[1]]
    ## long <- split[1]
    ## lat <- split[2]
    ## date <- split[3]
    setwd(paste(out.dir, folder,
                sep=""))
    ## files2 <- list.files()
    ## users <- sapply(files2[!files2%in%"out.RData"], function (x) strsplit(x, "_")[[1]][3])
    load("out.RData")
    data2 <- out ## rbind(data2, out)
    if (nrow(data2) > 1) {
        data2 <- as.data.frame(sapply(data2, function(x) gsub("\"|'|\t", "", x)))
    } else {
        data2 <- data.frame(lapply(sapply(data2, function(x) gsub("\"|'|\t", "", x)), function(x) t(data.frame(x))))
        }
    data2$grid <- rep(paste(strsplit(folder, "_")[[1]][1:2], collapse="_"), nrow(data2))
    setwd(out.dir)
    system(paste("rm -r ", folder,
                 sep=""))
    write.table(data2,paste(top.dir, "ChinaHKInstagramData_part_fixed.tsv",sep=""), row.names=F, append=T, col.names=F, sep="\t")
    if(i%%100==0) print(i)
}


bydate <- tapply(data$num, data$date, sum)
## plot(as.Date(names(bydate)), bydate, pch=16, ylim=c(5000,15000),
##      main="China Total Instagram Posts", xlab="Date", ylab="Total Posts")
## lines(c(as.Date("2014-09-29"),as.Date("2014-09-29")), c(0,100000), lty=2)

numusers <- NULL
dates <- as.Date(names(bydate))
for(i in 1:length(dates)){
    sub <- data[as.Date(data$date)==dates[i],]
    users <- unlist(sapply(sub$users, function (x) strsplit(as.character(x),
                                                             ",")[[1]]))
    numusers[i] <- length(unique(users))
}

## pdf("../figs/ChinaInstagramUsers.pdf")
plot(dates, numusers, pch=16, ylim=c(0,1500),
     ylab="Number of Unique Users",
     xlab="Date", main="China Total Instagram Users")
lines(c(as.Date("2014-09-29"),as.Date("2014-09-29")), c(0,100000), lty=2)
## dev.off()
