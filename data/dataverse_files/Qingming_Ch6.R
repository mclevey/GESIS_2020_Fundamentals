#Replication of Qingming Results, Chapter 6.  Data from Crimson Hexagon.
library(gdata)
qingming <- read.xls("QingmingBaiduSohuSina.xls")
martyr <- read.xls("QingmingANDMartyrsBaiduSohuSina.xls")

martyr$date <- as.Date(martyr$Date..UTC., format="%m/%d/%Y")
qingming$date <- as.Date(qingming$Date..UTC., format="%m/%d/%Y")

plot(martyr$date, martyr$Total.Posts/qingming$Total.Posts,
     col="white", ylim=c(0,.05), xlim=c(as.Date("2012-01-01"),
                                    as.Date("2016-06-01")),
                                    xlab="Date",
                                    ylab="Proportion of Qingming Posts that Contain the Word `Martyr'",
      xaxt="n")

years <- seq(2012,2016)
for(i in 1:length(years)){
    qmsub <- qingming[qingming$date<as.Date(paste(years[i], "-04-07", sep=""))
                      & qingming$date>as.Date(paste(years[i],
                                                    "-04-01",
                                                    sep="")),]
    msub <- martyr[martyr$date<as.Date(paste(years[i], "-04-07", sep=""))
                      & martyr$date>as.Date(paste(years[i],
                                                    "-04-01",
                                                    sep="")),]
    points(qmsub$date, msub$Total.Posts/qmsub$Total.Posts,
           pch=16)
    linet <- t.test(msub$Total.Posts/qmsub$Total.Posts[qmsub$Total.Posts!=0])
    lines(c(as.Date(paste(years[i],"-04-20", sep="")),
            as.Date(paste(years[i],"-04-20", sep=""))),
          c(linet$conf.int[1],linet$conf.int[2]))
    points(as.Date(paste(years[i],"-04-20", sep="")),
           linet$estimate, pch=17)
}
axis(1, at=seq(as.Date("2011-04-04"), as.Date("2016-04-04"),
              by="year"), label=seq(2011,2016))

