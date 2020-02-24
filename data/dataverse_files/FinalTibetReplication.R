#Replication of Figure 5.1 and 5.2, Roberts (2018)

tibet <- read.csv("TibetData.csv")
tibet$Date <- as.Date(tibet$Date, format="%m/%d/%Y")

#Figure 5.1
plot(tibet$Date, tibet$numafterdate, 
     xlim=c(as.Date("2011-01-01"),
            as.Date("2013-08-01")), 
            yaxs="i", 
     ylab="Number of Social Media Posts in Sample After Event",
     xlab="Date", ylim=c(0,100), col="white", cex.lab=1.3, cex.axis=1.5) 

#Lines for each event
for(i in 1:nrow(tibet)){
  lines(c(tibet[i,"Date"], tibet[i,"Date"]), 
        c(0, tibet[i,"numafterdate"]), lwd=5, col="darkgray")
}


#Plot of weekends versus weekdays, Figure 5.2
par(mfrow=c(2,1))
hist(tibet$numafterdate[tibet$weekend==1], col="darkgray", xlim=c(0,100),
     breaks=40, xlab="Number of Social Media Posts in Sample After Event",
     main="", ylim=c(0,30))
text(40,10, "Weekends", cex=1.5)
hist(tibet$numafterdate[tibet$weekend==0], col="darkgray", xlim=c(0,100),
     breaks=10, xlab="Number of Social Media Posts in Sample After Event",
     main="", ylim=c(0,30))
text(30,10, "Weekdays", cex=1.5)


#Negative Binomial Model, for Appendix
library(MASS)
library(stargazer)
lm.0 <- glm.nb(numafterdate ~ weekend, 
               data=tibet)
lm.1 <- glm.nb(numafterdate ~ weekend + Age.numeric  + Monk + Died, 
               data=tibet)
stargazer(lm.0,lm.1)
betas <- mvrnorm(1000, lm.1$coefficients, vcov(lm.1))
control <- c(1,0,mean(tibet$Age.numeric, na.rm=T), median(tibet$Monk, na.rm=T), median(tibet$Died, na.rm=T))
treat <- control
treat[2] <- 1

cmeans <- exp(betas%*%control)
tmeans <- exp(betas%*%treat)
csims <- rnbinom(1000, size=lm.1$theta, mu=cmeans)
tsims <- rnbinom(1000, size=lm.1$theta, mu=tmeans)

p1 <- hist(cmeans)
p2 <- hist(tmeans, breaks=20)
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,15), xlab="Distribution of Mean Blogs in Burst", yaxt="n", ylab="", main="")  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,15), add=T)
legend(10, 210, c("Weekday", "Weekend"), col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), lwd=5)

      