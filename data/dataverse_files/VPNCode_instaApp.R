#Replication of Figure 5.12, VPN Express Download Rank
#Statistics from App Annie, (must be logged in) see
#https://www.appannie.com/apps/ios/app/vpn-express-best-mobile-vpn-for-blocked-websites-online-games-version-5/rank-history/#vtype=day&countries=CN&start_date=2014-01-01&end_date=2015-06-01&device=iphone&view=rank&lm=3
data <- read.csv("VPNExpressRanks.csv")
data$date <- as.Date(data$date)

data <- data[order(data$date),]
plot(data$date, data$rank, ylim=c(1500,0), pch=16, xlab="Date", 
     ylab="VPN Express iPhone Rank", cex=.5,xaxt="n", cex.lab=1.3, cex.axis=1.2)
axis(1,at=c(as.Date("2014-05-01"), as.Date("2014-09-01"), as.Date("2015-01-01"), as.Date("2015-05-01")),
     labels=c("May 2014", "Sept 2014", "Jan 2015", "May 2015"), cex.lab=1.3, cex.axis=1.2)
lines(data$date, data$rank)
lines(c(as.Date("2014-05-31"), as.Date("2014-05-31")), c(100,900), lty=2)
text(as.Date("2014-05-31"), 75, "Google \n Block")
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), c(50,900), lty=2)
text(as.Date("2014-09-29"), 25, "Instagram \n Block")
lines(c(as.Date("2015-05-19"), as.Date("2015-05-19")), c(70,900), lty=2)
text(as.Date("2015-05-19"), 50, "Wikipedia \n Block")