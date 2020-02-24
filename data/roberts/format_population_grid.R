library(sp)
library(raster)
library(rgdal)
library(rworldmap)
library(rworldxtra)

countriesSP <- getMap(resolution='low')

## rasterlist <-  list.files('gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-2010/', full.names=TRUE, pattern="tif")

## available online
r <- raster("gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-2010/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals_2010.tif")

r <- aggregate(r, fact = 3, fun = sum)

## plot(r)

r.pts <- rasterToPoints(r)

r.pts <- data.frame(r.pts)
names(r.pts) <- c("long","lat","pop")

save(r.pts, file="latitude_longitude_population_agg.RData")

load(file="latitude_longitude_population_agg.RData")

r.pts.cn <- subset(
    r.pts,
    lat >= 20.23
    & lat <= 53.55
    & long >= 73.81
    & long <= 134.75
)

pointsSP = SpatialPoints(r.pts.cn[,-3], proj4string=CRS(proj4string(countriesSP)))

indices <- over(pointsSP, countriesSP)
r.pts.cn$iso3 <- indices$ISO3

r.pts.cn.hk.only <- subset(r.pts.cn, iso3 %in% c("CHN","HKG"))

save(r.pts.cn.hk.only, file="latitude_longitude_population_agg_china_hk_only.RData")
