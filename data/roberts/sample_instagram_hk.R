#library(devtools)
#install_github("pablobarbera/instaR/instaR")

library(instaR)

out.dir <- "./"

load("insta_token2")

#Search over China
#Latitude: 1 deg = 111.2 km
#Longitude: 1 deg = 69.1703234283616*cos(.0174532925199433*latitude) miles

latby <- 1/111.2*2.5
#farthest north point hong knog (so as not to include shenzhen) 22.45
#farthest south point hk 22.17
#farthest north point in china 53.55 North
#farthest southern point in china 20.23 North
latit <- seq(22.17, 22.45, by=latby)

#farthest west point hk 113.84
#farthest east point beijing 114.41
#farthest east point in china 134.75
#farthest west point in china 73.81
longby <- 1/69.1703234283616*cos(.0174532925199433*53.55)*1.60934*2.5
longit <- seq(113.84, 114.41, by=longby)
dates <- seq(as.Date("2014-09-20"), as.Date("2014-10-07"), by="day")

set.seed(1234)
sampn <- length(longit)*length(latit)
longlatmat <- matrix(sample(1:sampn, sampn),nrow=length(longit),
                     ncol=length(latit))
rownames(longlatmat) <- longit
colnames(longlatmat) <- latit
    for(i in 1:sampn){
        for(k in 1:(length(dates)-1)){
            entry <- which(longlatmat==i, arr.ind=T)
            long <- as.numeric(rownames(longlatmat)[entry[1]])
            lat <- as.numeric(colnames(longlatmat)[entry[2]])
            setwd(out.dir)
            folder=paste(long, lat,  dates[k], sep="_")
            tsq <- tryCatch({out <- searchInstagram(lat=lat, lng=long, distance=5000,
                           token=token, n=100, folder=folder, sleep=5,
                       mindate=dates[k], maxdate=dates[k+1])
            save(out, file=paste(out.dir, folder, "/out.RData", sep=""))
            system(paste("zip -r ", folder, ".zip ", folder,
                         sep=""))
            system(paste("rm -r ", folder,
                         sep=""))
                         }, error=function(error){
                             print(paste("MY_ERROR: ", folder, ":", error))}
                            )

        }
    }
#}

setwd(out.dir)
library(stringr)
files <- list.files()
i <- 1
unzip(files[i])
folder <- str_split(files[i], ".zip")[[1]][1]
split <- str_split(folder, "_")[[1]]
long <- split[1]
lat <- split[2]
date <- split[3]
setwd(paste(out.dir, folder,
            sep=""))
files2 <- list.files()
load("out.RData")
outout <- out
users <- sapply(files2[!files2%in%"out.RData"], function (x) str_split(x, "_")[[1]][3])
data <- data.frame(long=long, lat=lat, date=date,
                   num=length(files2[!files2%in%"out.RData"]),
                   users=paste(unique(users), collapse=","))
setwd(out.dir)
system(paste("rm -r ", folder,
             sep=""))

for(i in 1:length(files)){
    unzip(files[i])
    folder <- str_split(files[i], ".zip")[[1]][1]
    split <- str_split(folder, "_")[[1]]
    long <- split[1]
    lat <- split[2]
    date <- split[3]
    setwd(paste(out.dir, folder,
                sep=""))
    files2 <- list.files()
    if("out.Rdata"%in%files2){
        load("out.RData")
        outout <- rbind(outout,out)
    }
    users <- sapply(files2[!files2%in%"out.RData"], function (x) str_split(x, "_")[[1]][3])
    data <- rbind(data, data.frame(long=long, lat=lat, date=date,
                       num=length(files2[!files2%in%"out.RData"]),
                             users=paste(unique(users), collapse=",")))
    setwd(out.dir)
    system(paste("rm -r ", folder,
                 sep=""))
    if(i%%100==0) print(i)
}

#write.csv(data,"../data/HongKongInstagramCounts.csv", row.names=F)
#write.csv(outout,"../data/HongKongInstagramMeta.csv", row.names=F)
