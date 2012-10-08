library("ggplot2")
library("ggmap")
library("animation")

stations <- read.csv("stations.csv")
trips <- read.csv("trips.csv")

trips$startTime <- as.POSIXlt(trips$start_date)
trips$endTime <- as.POSIXlt(trips$end_date)

bikes <- NULL

for (station in stations$id){
    print(station)
    outBikes <- trips[trips$start_station_id==station]
    outBikes <- outBikes[!is.na(outBikes$startTime),]
    inBikes <- trips[trips$end_station_id==station]
    inBikes <- inBikes[!is.na(inBikes$endTime),]

    frame <- data.frame(time=c(outBikes$startTime,
                               inBikes$endTime),
                        bikes=c(rep(-1,nrow(outBikes)),
                                rep(1,nrow(inBikes))))

    frame <- frame[order(frame$time),]

    b <- data.frame(station=station,
                    time=frame$time,
                    bikes=cumsum(frame$bikes))

    b <- aggregate(b$bikes,
                   by=list(format(b$time,"%H"),
                   format(b$time,"%A")),
                   FUN=mean)
    names(b) <- c("hour","weekday","bikes")
    b$station <- station

    bikes <- rbind(bikes,b)
}

##qplot(weekday, bikes, data=bikes, colour=as.factor(station), group=station, geom="path")

border <- .01
nlat <- range(stations$lat)
lat[1] <- lat[1] - border
lat[2] <- lat[2] + border

lng <- range(stations$lng)
lng[1] <- lng[1] - border
lng[2] <- lng[2] + border

map <- get_map(location = c(lng[1],lat[1],lng[2],lat[2]),
               maptype = "watercolor",
               messaging = FALSE, urlonly = FALSE,
               filename = "boston", crop = TRUE,
               color = "color",
               source = "stamen")

bikeRange <- range(bikes$bikes)
bikeRangeFrame <- data.frame(lng=lng[1], lat=lat[1], bikes=bikeRange)

generateFrames <- function(){
    for (day in c("Monday","Tuesday","Wednesday",
                  "Thursday","Friday","Saturday","Sunday")){
        for (hour in c("00","01","02","03","04","05","06","07",
                       "08","09","10","11","12","13","14","15",
                       "16","17","18","19","20","21","22","23")){
            print(paste(day, hour))
            hourData <- bikes[bikes$weekday==day & bikes$hour == hour,]
            hourData <- merge(hourData,stations,by.x="station",by.y="id", all.y=TRUE)
            if (sum(is.na(hourData$bikes)) > 0){
                0 -> hourData[is.na(hourData$bikes),]$bikes
            }

            print(ggmap(map) +
                  geom_point(aes(x=lng, y=lat, size=bikes),
                             data=hourData) +
                  geom_point(aes(x=lng, y=lat, size=bikes),
                             data=bikeRangeFrame) +
                  scale_area() +
                  labs(title=paste(day, hour)))
        }
    }
}

## saveVideo({generateFrames()},
##           video.name="./dayofweek.mp4",
##           ffmpeg= "C:/Program Files/ffmpeg-20121005-git-d9dfe9a-win64-static/bin/ffmpeg")

saveSWF({generateFrames()},
        swf.name="dayofweek.swf",
        img.name="frame")
