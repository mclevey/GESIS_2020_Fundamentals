#Please contact authors about data availability
zh <- read.csv("../data/TwitterWeiboText.csv")

#Mutual information
doc.to.tdm <- function(documents, vocab){
        tdm <- matrix(0,nrow=length(documents),
                      ncol=length(vocab))
        for(i in 1:length(documents)){
            tdm[i,documents[[i]][1,]] <-
                documents[[i]][2,]
        }
        return(tdm)
    }

processed <- textProcessor(zh$tweet_textseg, zh,
                                                      wordLengths=c(2,Inf),removestopwords=T,
                                                      lowercase=F,
                                                      stem=F)
out <- prepDocuments(processed$documents, processed$vocab,
                                                               processed$meta)

tdm <- doc.to.tdm(out$documents, out$vocab)   

np <- sum(out$meta$weibo==1)
ns <- sum(out$meta$weibo==0)
D = np + ns
m <- t(tdm)
nj <- apply(m,1,function (x) sum(x>0))
nnotj <- apply(m,1,function (x) sum(x==0))
njp <- apply(m[,out$meta$weibo==1], 1, function (x) sum(x>0))
njs <- apply(m[,out$meta$weibo==0], 1, function (x) sum(x>0))
nnotjp <- apply(m[,out$meta$weibo==1], 1, function (x)
    sum(x==0))
nnotjs <- apply(m[,out$meta$weibo==0], 1, function (x)
    sum(x==0))

mi <- njp/D*log((njp*D)/(np*nj),2)+ njs/D*log((njs*D)/(nj*ns),2) +
          nnotjp/D*log((nnotjp*D)/(np*nnotj),2) +
                        nnotjs/D*log((nnotjs*D)/(nnotj*ns),2)
names(mi) <- out$vocab   


#Twitter words
twitterwords <- sort(mi[njp/np-njs/ns<0], decreasing=T)[1:25]
twittertranslate <- c("i'm", "released", "Hong Kong", "just",
                      "photos", "Beijing", "good", "the", "night",
                      "center", "peace", "Apple", "day", "my",
                      "international", "will", "citizens",
                      "government", "China", "happy", "the people",
                      "see", "freedom", "country", "like")
#Weibo words
weibowords <- sort(mi[njp/np-njs/ns>0], decreasing=T)[1:25]
weibotranslate <- c("myself", "know", "today", "play", "life",
                    "work hard", "others", "watch", "mv", "time",
                    "what", "forever", "through", "happy", "happy",
                    "persistence", "like", "none", "situation", "we",
                    "stars", "birthday", "think", "in my heart", "now")

library(xtable)
matrix <- cbind(paste("cntext{", names(twitterwords), "}: ",
                twittertranslate, sep=""),paste("cntext{", names(weibowords),
                                       "}: ", weibotranslate, sep=""))
colnames(matrix) <- c("Words Associated with Twitter",
                      "Words Associated with Weibo")


xtable(matrix)
