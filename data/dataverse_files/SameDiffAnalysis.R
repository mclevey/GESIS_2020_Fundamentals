#This document analyzes whether users click on the same/different post
#in the online experiment described in Chapter 4.
#NOTE: The data differs from that in "Fear, Friction, and Flooding: Methods of Online Information Control"
#because only the first censorship treatment was used to avoid post-treatment bias.
#The results and conclusions do not differ.

fvmerge <- read.csv("OnlineExperimentSameDiff.csv")
#Balance plot
plot(c(0,0), col="white", xlim=c(-.5,.5), ylim=c(0,5),
     xlab="Mean Difference (Treated-Control)", ylab="", yaxt="n")
lines(c(0,0), c(-1,10), lty=2)
points(gender$estimate[1]-gender$estimate[2],4, pch=16)
lines(c(gender$conf.int[1], gender$conf.int[2]),c(4,4), lwd=2)
text(gender$conf.int[2]+.1, 4, "Male")
points(han$estimate[1]-han$estimate[2],3, pch=16)
lines(c(han$conf.int[1], han$conf.int[2]),c(3,3), lwd=2)
text(han$conf.int[2]+.1, 3, "Han")
points(rural$estimate[1]-rural$estimate[2],2, pch=16)
lines(c(rural$conf.int[1], rural$conf.int[2]),c(2,2), lwd=2)
text(rural$conf.int[2]+.1, 2, "Born in \n Rural Village")
points(foreign$estimate[1]-foreign$estimate[2],1, pch=16)
lines(c(foreign$conf.int[1], foreign$conf.int[2]),c(1,1), lwd=2)
text(foreign$conf.int[2]+.1, 1, "Has Foreign \n Friends")

downmu <- t.test(fvmerge$SameDiff[fvmerge$Down==1])$estimate
downci <- t.test(fvmerge$SameDiff[fvmerge$Down==1])$conf.int
upmu <- t.test(fvmerge$SameDiff[fvmerge$Down==0])$estimate
upci <- t.test(fvmerge$SameDiff[fvmerge$Down==0])$conf.int
diffmu <- t.test(fvmerge$SameDiff[fvmerge$Down==1],fvmerge$SameDiff[fvmerge$Down==0])$estimate
diffci <- t.test(fvmerge$SameDiff[fvmerge$Down==1],fvmerge$SameDiff[fvmerge$Down==0])$conf.int

#Figure 4.5
plot(c(0,0), col="white", xlim=c(-.1,1), ylim=c(1,3), 
     ylab="", yaxt="n", xlab="Probability of Clicking on Same Topic")
points(downmu, 2.5, pch=16)
lines(downci, c(2.5,2.5))
text(.8, 2.6, "Treated")
points(upmu, 2, pch=16, col="grey25")
lines(upci, c(2,2), col="grey25")
text(.2, 2.1, "Control", col="grey25")
points(diffmu[1]-diffmu[2], 1.5, pch=16,col="gray15")
lines(diffci, c(1.5,1.5), col="gray15")
text(.65, 1.6, "Treated -  Control", col="grey15")
lines(c(0,0), c(-100,100), lty=2)


#Total posts clicked on after censorship
#Add one to uncensored users because they read the initial post
t.test(fvmerge$NumSameAfterNotCensored[fvmerge$UpDown==1]+1, 
       fvmerge$NumSameAfterNotCensored[fvmerge$UpDown==0])
