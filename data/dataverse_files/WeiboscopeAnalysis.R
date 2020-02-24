#This document contains the code for the user matching analysis from Chapter 4
#Relies on data collected by King-wa Fu, CH Chan, Michael Chau. Assessing Censorship on Microblogs in China: Discriminatory Keyword Analysis and Impact Evaluation of the 'Real Name Registration' Policy. IEEE Internet Computing. 2013; 17(3): 42-50. http://doi.ieeecomputersociety.org/10.1109/MIC.2013.28
library(data.table)
library(stringr)
library(lubridate)

######################################
#Part 1:
#Read in matches created from before (for full Weiboscope data, see https://hub.hku.hk/cris/dataset/dataset107483)
#Remove matches where control doesn't exist
#K2K match
#######################################

m.data <- fread("MatchedFinalExactZeros.csv")
m.data$censored <- m.data$treat

#Take out matches where control post does not exist
takeout <- m.data$strata[which(!m.data$deleted_last_seen==""& m.data$censored==0)]
m.data <- m.data[!m.data$strata%in%takeout,]

#Load user data for matched users posts before/after treatment
matchedusersseg <- fread("MatchedUsers_Seg.csv")

#Check out matched text examples
tapply(m.data$text, m.data$strata, unique)

#K2K matching -- we only want one match per treated unit
#Taken from CEM package (package K2K wasn't functioning)
matches <- NULL
bigstrat <- as.numeric(names(table(m.data$strata))[table(m.data$strata)>2])
for(i in 1:length(bigstrat)){
  sub <- as.data.frame(m.data[m.data$strata==bigstrat[i],])
  n.tr <- sum(sub$treat==1)
  n.ct <- sum(sub$treat==0)
  m <- min(n.tr, n.ct)
  mat <- as.matrix(dist(sub[,c("tendaybcensor", "tendaybpost",
                               "fivedaybcensor", "fivedaybpost",
                               "censoredratebefore",
                               "postratebefore", "fivedaybexist",
                               "tendaybexist", "existratebefore")]))
  colnames(mat) <- rownames(mat) <- sub$mid
  if(n.tr<n.ct){
    controls <- which(sub$treat==0)
    for(j in 1:n.tr){
      treati <- which(sub$treat==1)[j]
      mins <- apply(mat[,controls],1,function
                    (x) which(x==min(x, na.rm=T)))[treati]
      matches <- c(matches, colnames(mat[,controls])[unlist(mins)[1]], rownames(mat)[treati])
      controls <- controls[-unlist(mins)[1]]
    }
  }
  if(n.tr>n.ct){
    controls <- which(sub$treat==1)
    for(j in 1:n.ct){
      treati <- which(sub$treat==0)[j]
      mins <- apply(mat[,controls],1,function
                    (x) which(x==min(x, na.rm=T)))[treati]
      matches <- c(matches,
                   colnames(mat[,controls])[unlist(mins)[1]],
                   rownames(mat)[treati])
      controls <- controls[-unlist(mins)[1]]
    }
  }
  print(i)
  print(length(matches))
  
}

#Takeout unmatched posts
takeout <- m.data$mid[m.data$strata%in%bigstrat &
                        !m.data$mid%in%matches]

m.data <- m.data[!m.data$mid%in%takeout,]
#174 matched posts
nrow(m.data)

#write.csv(m.data, "FinalDataK2K.csv", row.names=F)


######################################
#Part 2:
#Clean up text of matched posts
#Grab data from each user who is in the dataset
#Grab posts for each user 10 days before/10 days after matched post
#Clean up these posts
#######################################
library(data.table)
library(stringr)
#library(lubridate)
allsub <- matchedusersseg
allsub$date <- as.Date(allsub$created_at)
m.data <- read.csv("FinalDataK2K.csv")
m.data$date <- as.Date(m.data$created_at)

#Take text of censored post
i <- 2
sub <- allsub[allsub$mid==m.data$mid[i],]
#Clean up the text
sub$textseg <- paste(sub$textseg, " ", sep="")
sub$textseg <- gsub("/ /@u.*?：","",sub$textseg)
sub$textseg <- gsub("//@u.*?：","",sub$textseg)
sub$textseg <- gsub("@ u.*?：","",sub$textseg)
sub$textseg <- gsub("@u.*?：","",sub$textseg)
sub$textseg <- gsub("http : //.*? ","",sub$textseg)
sub$textseg <- gsub("[\x80-\xFF]","",sub$textseg)
sub$textseg <- gsub("\\[.*?\\]","",sub$textseg)
m.data$censoredtext[i] <- sub$textseg
m.data$censorednop[i] <- sub$text
for(i in c(1,3:nrow(m.data))){
  sub <- allsub[allsub$mid==m.data$mid[i],]
  #Clean up the text
  sub$textseg <- paste(sub$textseg, " ", sep="")
  sub$textseg <- gsub("/ /@u.*?：","",sub$textseg)
  sub$textseg <- gsub("//@u.*?：","",sub$textseg)
  sub$textseg <- gsub("@ u.*?：","",sub$textseg)
  sub$textseg <- gsub("@u.*?：","",sub$textseg)
  sub$textseg <- gsub("http : //.*? ","",sub$textseg)
  sub$textseg <- gsub("[\x80-\xFF]","",sub$textseg)
  sub$textseg <- gsub("\\[.*?\\]","",sub$textseg)
  m.data$censoredtext[i] <- sub$textseg
  m.data$censorednop[i] <- sub$text
}

#UserTimeline contains data about each user by date and how many posts were written and censored
#The censored variable is the number of "Permission Denied" posts, excluding "Weibo Does Not Exist"
users <- fread("UserTimeline.csv")
usersmatch <- users[users$user%in%m.data$uid,]

#Create a new dataset called keeptext to collect posts from each matched users
#keep data here is just to check to make sure we have all the posts
keep <- as.data.frame(matrix(NA, nrow=1, ncol=ncol(usersmatch)))
names(keep) <- names(usersmatch)
keeptext <- as.data.frame(matrix(NA, nrow=1, ncol=ncol(allsub)))
names(keeptext) <- names(allsub)
keeptext$diff <- NA
keep$diff <- NA
keeptext$treat <- NA
keep$treat <- NA
keeptext$strata <- NA
keeptext$censoredpost <- NA
#For each post in the matched dataset, collect all posts 
#that were in a 10 day window from taht suer
for(i in 1:nrow(m.data)){
  sub <- usersmatch[usersmatch$user==m.data$uid[i],]
  subtext <- allsub[allsub$uid==m.data$uid[i],]
  
  #Start with 10 days before
  keep1 <- sub[ymd(sub$date)< ymd(m.data$date[i]) &
                 ymd(sub$date)>
                 (ymd(m.data$date[i])-days(10)),]
  keeptext1 <- subtext[ymd(subtext$date)< ymd(m.data$date[i]) &
                         ymd(subtext$date)>
                         (ymd(m.data$date[i])-days(10)),]
  #check to make sure we have them all
  if(!(sum(keep1$num, na.rm=T)==nrow(keeptext1))) print(i)
  
  #Now for 10 days after
  keep2 <- sub[ymd(sub$date)> ymd(m.data$date[i]) &
                 ymd(sub$date)<
                 (ymd(m.data$date[i])+days(10)),]
  keeptext2 <- subtext[ymd(subtext$date)> ymd(m.data$date[i]) &
                         ymd(subtext$date)<
                         (ymd(m.data$date[i])+days(10)),]
  keep1$diff <- as.Date(keep1$date)-as.Date(m.data$created_at[i])
  keep2$diff <- as.Date(keep2$date)-as.Date(m.data$created_at[i])
  keeptext1$diff <-
    as.Date(keeptext1$date)-as.Date(m.data$created_at[i])
  keeptext2$diff <-
    as.Date(keeptext2$date)-as.Date(m.data$created_at[i])
  keeptext1$treat <- m.data$treat[i]
  keeptext2$treat <- m.data$treat[i]
  keeptext1$strata <- m.data$strata[i]
  keeptext2$strata <- m.data$strata[i]
  keeptext1$censoredpost <- m.data$censoredtext[i]
  keeptext2$censoredpost <- m.data$censoredtext[i]
  keep1$treat <- m.data$treat[i]
  keep2$treat <- m.data$treat[i]
  if(sum(keep1$num, na.rm=T)==nrow(keeptext1)){
    keep <- rbind(keep, as.data.frame(keep1), as.data.frame(keep2))
    keeptext <- rbind(keeptext, as.data.frame(keeptext1), as.data.frame(keeptext2))
  }
}

#Clean empty row from dataset
keep <- keep[-1,]
keeptext <- keeptext[-1,]

#Clean up keeptext user post text
keeptext$textseg <- paste(keeptext$textseg, " ", sep="")
keeptext$textseg <- gsub("/ /@u.*?：","",keeptext$textseg)
keeptext$textseg <- gsub("//@u.*?：","",keeptext$textseg)
keeptext$textseg <- gsub("@ u.*?：","",keeptext$textseg)
keeptext$textseg <- gsub("@u.*?：","",keeptext$textseg)
keeptext$textseg <- gsub("http : //.*? ","",keeptext$textseg)
keeptext$textseg <- gsub("[\x80-\xFF]","",keeptext$textseg)
keeptext$textseg <- gsub("\\[.*?\\]","",keeptext$textseg)

##########
#Part 3:
#Calculate Mutual Information for sensitive post analysis
#To do this, grab uncensored posts from users during the same time period
#Compare matched posts to uncensored posts same users, same time period
##########

#doc.to.tdm function necessary for MI
doc.to.tdm <- function(documents, vocab){
  tdm <- matrix(0,nrow=length(documents),
                ncol=length(vocab))
  for(i in 1:length(documents)){
    tdm[i,documents[[i]][1,]] <-
      documents[[i]][2,]
  }
  return(tdm)
}

library(stm)
matchedusersseg$textseg <- paste(matchedusersseg$textseg, " ", sep="")
matchedusersseg$textseg <- gsub("/ /@u.*?：","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("//@u.*?：","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("@ u.*?：","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("@u.*?：","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("http : //.*? ","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("[\x80-\xFF]","",matchedusersseg$textseg)
matchedusersseg$textseg <- gsub("\\[.*?\\]","",matchedusersseg$textseg)

notcensored <- matchedusersseg$textseg[matchedusersseg$deleted_last_seen==""]
notcensored <- notcensored[str_length(notcensored)>15]

set.seed(54321)
notcensored <- sample(notcensored, 10000)
all <- data.frame(text=c(unique(m.data$censoredtext), notcensored), 
                  censored=c(rep(1, length(unique(m.data$censoredtext))), rep(0, length(notcensored))))
processed <- textProcessor(all$text, metadata=all, wordLengths = c(2,Inf))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

tdm <- doc.to.tdm(out$documents, out$vocab)

#Mutual information
np <- sum(out$meta$censored==1)
ns <- sum(out$meta$censored==0)
D = np + ns
m <- t(tdm)
nj <- apply(m,1,function (x) sum(x>0))
nnotj <- apply(m,1,function (x) sum(x==0))
njp <- apply(m[,out$meta$censored==1], 1, function (x) sum(x>0))
njs <- apply(m[,out$meta$censored==0], 1, function (x) sum(x>0))
nnotjp <- apply(m[,out$meta$censored==1], 1, function (x) sum(x==0))
nnotjs <- apply(m[,out$meta$censored==0], 1, function (x) sum(x==0))

mi <- njp/D*log((njp*D)/(np*nj),2)+ njs/D*log((njs*D)/(nj*ns),2) +
  nnotjp/D*log((nnotjp*D)/(np*nnotj),2) +
  nnotjs/D*log((nnotjs*D)/(nnotj*ns),2) 
names(mi) <- out$vocab
diff <- njp/np - njs/ns

library(rjson)
#Remove stopwords
stopwords <- fromJSON(readLines("~/Dropbox/FFFBook/Chapter5Analysis/TwitterWeiboComparison/zh_stopwords.json"))
mi <- mi[!names(mi)%in%stopwords & diff>0]
keepwords <- names(sort(mi, decreasing=T)[1:100])

################
#Part 4:
#For each post in keeptext, calculate string kernel similarity between that post and the matched post
#For each post, calculate the number of sensitive words in the post
################

library(kernlab)
library(stringdist)
#Put text and matched post into list format to make this faster
compare <- list()
for(i in 1:nrow(keeptext)){
  compare[[i]] <- list()
  #Remove commas, periods and brackets from the text so that "...."
  #doesn't create high similarity
  compare[[i]]$text <- gsub(" ", "",keeptext$textseg[i])
  compare[[i]]$text <- gsub("。", "",compare[[i]]$text)
  compare[[i]]$text <- gsub("【", "",compare[[i]]$text)
  compare[[i]]$textspace <- keeptext$textseg[i]
  compare[[i]]$censored <- gsub(" ", "",keeptext$censoredpost[i])
  compare[[i]]$censored <- gsub("。", "",compare[[i]]$censored)
  compare[[i]]$censored <- gsub("【", "",compare[[i]]$censored)
  compare[[i]]$censoredspace <- keeptext$censoredpost[i]
  if(i%%1000==0) print (i)
}

#String similarity function
sk <- stringdot(type="string", length=2)
skout <- lapply(compare, function (x) sk(x$text,x$censored))

#NA's indicate that the text for comparison is empty after preprocessing
#So the string similarity should be zero
skout[is.na(skout)] <- 0
keeptext$sk <- unlist(skout)

#Sensitive word analysis
sensitivewords <- sapply(keepwords, function (x)
  str_detect(keeptext$textseg, x))
#Normalize by postlength
keeptext$sensitive <-
  apply(sensitivewords,1,sum)/str_length(keeptext$text)

#Both string similarity to matched post and 
#including sensitive words sensitive predict censorship
summary(lm(noexist ~ sk, keeptext))
summary(lm(noexist ~ sensitive, keeptext))

################
#Part 5:
#Plot string kernel similarity, sensitive words, etc
#Do diff-in-diff analysis
################

#Function for calculating error bars later
err.bars <- function(data, var, day){
  data <- na.omit(data[,c("diff", var, "treat")])
  datasub <- data[data$diff==day,]
  ttest <- t.test(datasub[,var][datasub$treat==1], datasub[,var][datasub$treat==0])
  return(ttest$conf.int)
}

#Sensitive word plot, Figure 4.2
par(mai=c(1.02,1.15,0.82,0.42))
control <- tapply(keeptext$sensitive[keeptext$treat==0], keeptext$diff[keeptext$treat==0], mean, na.rm=T)
treat <- tapply(keeptext$sensitive[keeptext$treat==1], keeptext$diff[keeptext$treat==1], mean, na.rm=T)

plot(as.numeric(names(control)), treat- control, ylim=c(-.0008,.0008), pch=16,
     xlab="Days from Matched Post", ylab="Average Proportion Sensitive Words \n (Censored User - Uncensored User)", col="black")
for(i in c(seq(-9,-1), seq(1,9))){
  outtest <- err.bars(keeptext, "sensitive", i)
  lines(c(i,i), c(outtest[1], outtest[2]), col="darkgray")
}
lines(names(control)[1:9],  rep(mean((treat- control)[1:9]),9), col="black", lty=3)
lines(names(control)[10:18],  rep(mean((treat- control)[10:18]),9), col="black", lty=3)
lines(c(0,0),c(-100,100), lty=2)

#Difference-in-difference estimates
summary(lm(sensitive ~ I(diff>0)*treat, data=keeptext))
summary(lm(sensitive ~ I(diff>0)*treat, data=keeptext))
summary(lm(sensitive ~ I(diff>0)*treat + image + week + factor(uid), data=keeptext))

#String Kernel Similarity Analysis, Figure 4.1
par(mai=c(1.02,1.15,0.82,0.42))
control <- tapply(keeptext$sk[keeptext$treat==0], keeptext$diff[keeptext$treat==0], mean, na.rm=T)
treat <- tapply(keeptext$sk[keeptext$treat==1], keeptext$diff[keeptext$treat==1], mean, na.rm=T)

plot(as.numeric(names(control)), treat- control,  ylim=c(-.02,.05), pch=16,
     xlab="Days from Matched Post", ylab="String Similarity to Matched Post \n (Censored User - Uncensored User)", col="black")
for(i in c(seq(-9,-1), seq(1,9))){
  outtest <- err.bars(keeptext, "sk", i)
  lines(c(i,i), c(outtest[1], outtest[2]), col="darkgray")
}
lines(names(control)[1:9],  rep(mean((treat- control)[1:9]),9), col="black", lty=3)
lines(names(control)[10:18],  rep(mean((treat- control)[10:18]),9), col="black", lty=3)

lines(c(0,0),c(-100,100), lty=2)

#Diff & Diff
summary(lm(sk ~ I(diff>0)*treat, data=keeptext))
summary(lm(sk ~ I(diff>0)*treat + image + week + factor(uid), data=keeptext))

#Censored Plot, Figure 4.3
#Calculate for Weibo Does not exist message rather than permission denied to get more leverage
keeptext$exist <- as.numeric(keeptext$deleted_last_seen=="")
keeptext$noexist <- as.numeric(!as.logical(keeptext$exist))

par(mai=c(1.02,1.15,0.82,0.42))
control <- tapply(keeptext$noexist[keeptext$treat==0], keeptext$diff[keeptext$treat==0], mean, na.rm=T)
treat <- tapply(keeptext$noexist[keeptext$treat==1], keeptext$diff[keeptext$treat==1], mean, na.rm=T)

plot(as.numeric(names(control)), treat- control,  ylim=c(-.05,.1), pch=16,
     xlab="Days from Matched Post", ylab="Proportion Censored \n (Censored User - Uncensored User)", col="black")
#points(as.numeric(names(treat)), treat, col="black", pch=15)
for(i in c(seq(-9,-1), seq(1,9))){
  outtest <- err.bars(keeptext, "noexist", i)
  lines(c(i,i), c(outtest[1], outtest[2]), col="darkgray")
}
lines(names(control)[1:9],  rep(mean((treat- control)[1:9]),9), col="black", lty=3)
lines(names(control)[10:18],  rep(mean((treat- control)[10:18]),9), col="black", lty=3)

lines(c(0,0),c(-100,100), lty=2)

summary(lm(noexist ~ I(diff>0)*treat, data=keeptext))
summary(lm(noexist ~ I(diff>0)*treat + image + week + factor(uid), data=keeptext))

#######
#Part 6
#Code a random set of posts on days 2,3 & 4
#Code a variety of things about each post
#######
results <- read.csv("CodedPosts.csv")
#Merge with keeptext from days 2,3 & 4
tomerge <- keeptext[keeptext$diff%in%c(2,3,4),]
mergedresults <- merge(results, tomerge, by.x="mid", by.y="mid")

#Human similarity aligns with string similarity and sensitivity scores
tapply(mergedresults$sk, mergedresults$similarity, mean, na.rm=T)
tapply(mergedresults$sensitive, mergedresults$similarity, mean, na.rm=T)

cor.test(mergedresults$sk, mergedresults$similarity)
cor.test(mergedresults$sensitive, mergedresults$similarity)

#Make coding numeric
mergedresults$politics1 <- as.numeric(mergedresults$politics=="Yes")
mergedresults$complaining1 <-
  as.numeric(mergedresults$complaining=="Yes")

#More complaining in treated group after censorship about censorship, 
#though low numbers of complaining about censorship in general mean not significant
table(mergedresults$complaining1, mergedresults$treat)[2,]/table(mergedresults$treat)

#Treated group talks more about politics after
t.test(mergedresults$politics1[mergedresults$treat==1],
       mergedresults$politics1[mergedresults$treat==0])
#Treated group complains more after
t.test(mergedresults$complaining1[mergedresults$treat==1],
       mergedresults$complaining1[mergedresults$treat==0])
#Treated group has more similar posts after
t.test(mergedresults$similarity[mergedresults$treat==1],
       mergedresults$similarity[mergedresults$treat==0])

#####################
#Part 7 Case studies#
#####################

#User Zhang
strat <- keeptext[keeptext$strata==4405,]
treated <- strat[strat$treat==1,]
control <- strat[strat$treat==0,]
sum(str_detect(treated$text[treated$diff>0], "网络"))
sum(str_detect(control$text[control$diff>0], "网络"))

strat$text[strat$diff>0 & strat$treat==1][57]
strat$text[strat$diff>0 & strat$treat==1][580]

#User Zhu
uid1 <- keeptext$uid[keeptext$mid=="mAMYCGfLWI"]
usertime <- keeptext[keeptext$uid==uid1,]
summary(usertime$exist)
t.test(usertime$sk[usertime$diff<0], usertime$sk[usertime$diff>0])
t.test(usertime$noexist[usertime$diff<0], usertime$noexist[usertime$diff>0])
keeptext[keeptext$mid=="mAMYCGfLWI",]

#User Zhu's control
uid2 <- unique(keeptext$uid[keeptext$strata==unique(usertime$strata) &
                              keeptext$treat==0])
usertime2 <- keeptext[keeptext$uid==uid2,]
t.test(usertime2$sk[usertime2$diff<0], usertime2$sk[usertime2$diff>0])
t.test(usertime2$noexist[usertime2$diff<0], usertime2$noexist[usertime2$diff>0])




