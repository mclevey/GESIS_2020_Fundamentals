setwd("../data")

## this code just plots data from App Annie . com.  We used WebPlotDigitizer to create our own plot.
##Data reprinted with permission from App Annie.

#Twitter
pdf("../figs/AppAnnieTwitterAndFacebook.pdf", width=7, height=5)
par(mar=c(5, 6, 1, 2))
vpna <- read.csv("TwitterAppAnnieData_WebPlotDigitizer.csv", header=F, stringsAsFactors=F)
vpna <- vpna[order(as.Date(vpna$V2)),]
vpna$V2 <- sub(" ", "", vpna$V2)
## correct date
vpna$V2[which(as.character(vpna$V2)=="2014/9/25")][2] <- "2014/9/24"
plot(as.Date(vpna$V2), log(vpna$V1), ylim=log(c(800,90)),
     ylab=c("iPhone Application Rank in China"),
     xlab="Date (2014)", pch=16,
     bty="n", xaxt="n", cex.lab=1.5, col=rgb(80, 154, 193, maxColorValue=255),
     yaxt="n", cex=1.25
     )
axis(
    side=1,
    at=as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15")),
    labels=as.character(format(as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15")), "%b %d")),
    cex.axis=1.5
    )
axis(
    side=2,
    at=log(c(800, 400, 200, 100)),
    labels=c(800, 400, 200, 100),
    cex.axis=1.5
    )
#                                        lines(as.Date(vpna$V2), vpna$V1, ylim=rev(range(vpna$V1)))
##
vpna <- read.csv("FacebookAppAnnieData_WebPlotDigitizer.csv", header=F)
vpna <- vpna[order(as.Date(vpna$V2)),]
points(as.Date(vpna$V2), log(vpna$V1), pch=16, col=rgb(64, 87, 149, maxColorValue=255), cex=1.25)
##
text(as.Date("2014-09-29"), log(100), "Instagram Block", cex=1.5)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), log(c(900,110)), lty=3, lwd=4)
legend(x=as.Date("2014-09-15"), y=log(120), legend=rev(c("Twitter", "Facebook")), col=rev(c(rgb(80, 154, 193, maxColorValue=255), rgb(64, 87, 149, maxColorValue=255))), pch=16, bty="n", cex=1.5)
dev.off()




#VPN Artifact and Express
pdf("../figs/AppAnnieArtifactAndExpress.pdf", width=7, height=5)
par(mar=c(5, 6, 1, 2))
vpna <- read.csv("VPNArtifact_WebPlotDigitizer.csv", header=F)
vpna <- vpna[order(as.Date(vpna$V2)),]
artifact.col <- rgb(93, 216, 226, maxColorValue=255)
plot(as.Date(c("2014/9/15", as.character(vpna$V2)))[-length(vpna$V1)], log(vpna$V1), ylim=log(c(1600,4)),
     ## fixed dates!
     ylab=c("iPhone Application Rank in China"),
     xlab="Date (2014)", pch=16,
     bty="n", xaxt="n", cex.lab=1.5, col=artifact.col,
     yaxt="n", cex=1.25,
     xlim=as.Date(c("2014-09-15", "2014-10-15"))
     )
axis(
    side=1,
    at=as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15")),
    labels=as.character(format(as.Date(c("2014-09-01", "2014-09-15", "2014-09-29", "2014-10-15")), "%b %d")),
    cex.axis=1.5
    )
axis(
    side=2,
    at=log(c(1600, 400, 100, 25, 6)),
    labels=c(1600, 400, 100, 25, 6),
    cex.axis=1.5
    )
                                        #lines(as.Date(vpna$V2), vpna$V1, ylim=rev(range(vpna$V1)))
##
vpna <- read.csv("VPNExpressAppAnnieData_WebPlotDigitizer.csv", header=F)
vpna <- vpna[order(as.Date(vpna$V2)),]
vpna$V2 <- as.Date(vpna$V2)
## missing date, corrected
vpna$V2[vpna$V2>as.Date("2014/10/9")] <- vpna$V2[vpna$V2>as.Date("2014/10/9")]-1
express.col <- rgb(234, 205, 90, maxColorValue=255)
points(as.Date(vpna$V2), log(vpna$V1), pch=16, col=express.col, cex=1.25)
##
text(as.Date("2014-09-29"), log(5), "Instagram Block", cex=1.5)
lines(c(as.Date("2014-09-29"), as.Date("2014-09-29")), log(c(900,6)), lty=3, lwd=4)
legend(x=as.Date("2014-09-15"), y=log(20), legend=rev(c("VPN Artifact", "VPN Express")), col=rev(c(artifact.col, express.col)), pch=16, bty="n", cex=1.5)
dev.off()
