#Replication of Figure 5.10, Wikipedia Page Views

library(stringr)

#Aggregate Chinese language page views from http://stats.grok.se/
counts <- read.csv("countsbyhour.csv")

counts$date <- sapply(counts$File, function (x) str_split(x,
                                                          "-")[[1]][2])
counts$time <- sapply(counts$File, function (x) str_split(x,
                                                          "-")[[1]][3])
counts$time <- sapply(counts$time, function (x) str_split(x,
                                                          ".gz")[[1]][1])
counts$time <- sapply(counts$time, function (x) substring(x,
                                                          1,2))

counts$year <- sapply(counts$date, function (x) substring(x,1,4))
counts$month <- sapply(counts$date, function (x) substring(x,5,6))
counts$day <- sapply(counts$date, function (x) substring(x,7,8))
counts$Date <- paste(counts$year, "-", counts$month, "-",
                             counts$day," ", counts$time, ":00:00", sep="")
counts$datetime <- as.POSIXct(counts$Date, gz="UTC")
counts$datetime <- format(counts$datetime, tz="Hong Kong")
counts$Date <- as.Date(counts$datetime)
bydate <- tapply(counts$Requests, counts$Date, function (x) max(x))
#Alternatively sum
bydatesum <- tapply(counts$Requests, counts$Date, function (x) sum(x))

dates <- as.Date(names(bydate))
fitbjb <- smooth.spline(bydate[1:19] 
                        ~ as.numeric(dates)[1:19],
                        nknots=5)
fitbja <- smooth.spline(bydate[20:32] 
                        ~ as.numeric(dates)[20:32],
                        nknots=5)

#For More Info on Wikipedia Block
#http://www.forbes.com/sites/thomasbrewster/2015/05/22/wikipedia-disturbed-over-fresh-china-censorship/#66e7e0f25f84

#Total
#We are missing some of the hours in the first and last days, 
#so we will take those out.  
bydatesum <- tapply(counts$Requests, counts$Date, function (x) sum(x))
bydatesum <- bydatesum[-c(1,length(bydatesum))]
dates <- as.Date(names(bydatesum))
fitbjb <- smooth.spline(bydatesum[1:18] 
                        ~ as.numeric(dates)[1:18],
                        nknots=5)
fitbja <- smooth.spline(bydatesum[19:30] 
                        ~ as.numeric(dates)[19:30],
                        nknots=5)

plot(as.Date(names(bydatesum)), bydatesum, pch=16, xlab="Date (2015)",
     ylab="Total Chinese Language Wikipedia Page Views", yaxt="n")
axis(2, at=seq(8000000, 15000000, by=1000000), c("8m", "","10m","","12m", "", "14m", ""))
lines(c(as.Date("2015-05-19"), as.Date("2015-05-19")), c(0,13000000),
      lty=2)
text(as.Date("2015-05-19"), 14250000, "Wikipedia \n Block")
lines(as.Date(fitbja$x, origin="1970-01-01"), fitbja$y)
lines(as.Date(fitbjb$x, origin="1970-01-01"), fitbjb$y)

