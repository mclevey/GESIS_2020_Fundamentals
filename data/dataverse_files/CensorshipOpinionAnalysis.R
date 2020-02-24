#This document analyzes whether users who were treated at some point
#during the experiment had different opinions about censorship after being censored
#Note that there are more treated units than in the Same/Diff analysis
#because treatment can occur at any time during the experiment, rather than
#just the first post.

blogrollall <- read.csv("CensorshipOpinions.csv")

drawlines <- function(var, level, lab){
  lines(c(1, mean(var[blogrollall$treat==1], na.rm=T)), c(level, level), lwd=6)
  lines(c(1, mean(var[blogrollall$treat==0], na.rm=T)), c(level-.25, level-.25), lwd=6, col="darkgrey")
  text(mean(var[blogrollall$treat==0], na.rm=T)+.1, level, lab, adj=0)
}
plot(c(0,0), col="white", xlim=c(1,6), ylim=c(.5,9.5), 
     ylab="", yaxt="n", xlab="Should be Regulated (1-5 Scale)", xaxt="n")
drawlines(blogrollall$uncivil, 7, "Uncivilized Language")
drawlines(blogrollall$false, 1, "False Info")
drawlines(blogrollall$rumors, 5, "Rumors")
drawlines(blogrollall$spam, 3, "Spam")
drawlines(blogrollall$games, 8, "Games")
drawlines(blogrollall$chat, 9, "Online \n Discussion")
drawlines(blogrollall$ads, 6, "Ads")
drawlines(blogrollall$violence, 2, "Violence")
drawlines(blogrollall$porn, 4, "Pornography")
axis(1, at=seq(1,5), seq(1,5))
legend(4,9.5, c("Treated", "Control"), col=c("black", "darkgrey"), lwd=6)

blogrollall$shouldcensor <- (blogrollall$porn + blogrollall$violence + 
  blogrollall$ads + blogrollall$chat + blogrollall$games + blogrollall$spam + 
  blogrollall$rumors + blogrollall$false + blogrollall$uncivil)/45

t.test(blogrollall$shouldcensor[blogrollall$treat==1], 
       blogrollall$shouldcensor[blogrollall$treat==0])
