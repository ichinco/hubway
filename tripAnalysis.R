library("ggplot2")
library("ggmap")
library("animation")

stations <- read.csv("stations.csv")
trips <- read.csv("trips.csv")

days <- c("Monday","Tuesday","Wednesday",
          "Thursday","Friday","Saturday","Sunday")

hours <- c("00","01","02","03","04","05","06","07",
           "08","09","10","11","12","13","14","15",
           "16","17","18","19","20","21","22","23")

trips$startTime <- as.POSIXlt(trips$start_date)
trips$endTime <- as.POSIXlt(trips$end_date)
trips$startHour <- format(trips$startTime,"%H")
trips$startDay <- format(trips$startTime,"%A")
trips$endHour <- format(trips$endTime,"%H")
trips$endDay <- format(trips$endTime,"%A")

## destinationAggregation <- aggregate(trips,
##                                     by=list(trips$start_station_id,
##                                             trips$end_station_id,
##                                             format(trips$startTime,"%H"),
##                                             format(trips$endTime,"%H"),
##                                             format(trips$startTime,"%A"),
##                                             format(trips$endTime,"%A")),
##                                     FUN=length)
## names(destinationAggregation) <- c("startStation",
##                                    "endStation",
##                                    "startHour",
##                                    "startDay",
##                                    "endHour",
##                                    "endDay")

destinationAggregation <- NULL
for (i in seq(1,nrow(trips))){
    t <- destinationAggregation[trips[i,]$start_station_id==destinationAggregation$startStation &
                                trips[i,]$end_station_id==destinationAggregation$endStation &
                                trips[i,]$startHour==destinationAggregation$startHour &
                                trips[i,]$endHour==destinationAggregation$endHour &
                                trips[i,]$startDay==destinationAggregation$startDay &
                                trips[i,]$endDay==destinationAggregation$endDay,]

    if (!is.null(t)){
        t$trips <- t$trips + 1
    } else {
        frame <- data.frame(startStation=trips[i,]$start_station_id,
                            endStation=trips[i,]$end_station_id,
                            startHour=trips[i,]$startHour,
                            endHour=trips[i,]$endHour,
                            startDay=trips[i,]$startDay,
                            endDay=trips[i,]$endDay,
                            trips=1)
        destinationAggregation <- rbind(frame,
                                        destinationAggregation)
    }

    if (i %% 100 == 0){
        print(i)
    }
}

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

generateFrames <- function(){
    for (day in days){
        for (hour in hours) {
            print(paste(day, hour))

            relevantTrips <- destinationAggregation[(destinationAggregation$startDay==day
                                                     & destinationAggregation$startHour==hour)
                                                    | (destinationAggregation$endDay==day
                                                       & destinationAggregation$endHour==hour),]

            segments <- NULL

            for (startStation in stations$id) {
                for (endStation in stations$id) {
                    line <- relevantTrips[relevantTrips$startStation==startStation &
                                          relevantTrips$endStation==endStation,]
                    if (!is.null(line)){
                        start <- stations[stations$id==startStation,]
                        end <- stations[stations$id==endStation,]
                        segment <- geom_segment(aes(x=start$lng,
                                                    y=start$lat,
                                                    xend=c(tail(end$lng, n=-1), NA),
                                                    yend=c(tail(end$lat, n=-1), NA)),
                                                arrow=arrow(length=unit(0.3,"cm")),
                                                size=.5*line$trips)

                        segments <- segments + segment
                    }
                }
            }

            print(ggmap(map) +
                  geom_point(aes(x=lng, y=lat),data=stations) +
                  scale_area() +
                  segments +
                  labs(title=paste(day, hour)))
        }
    }
}
