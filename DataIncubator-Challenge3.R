#' This script is for data incubator challenge (Data Project)
#' Data: United States Department of Transportation, Office of the Assistant Secretary for Research and Technology
#'        Bureau of Transportation Statistics
#' http://www.transtats.bts.gov/Tables.asp?DB_ID=120&DB_Name=Airline%20On-Time%20Performance%20Data&DB_Short_Name=On-Time
#' I automatically scraped the data from the web and selected useful features 
#' I then constructed several new features to see the flight performance by airline, hubs, and time 
#' 
#' Date: 10/29/2016
#' Author: Ying Yang



#######################transferring data from AWS to mySQL local database#########################

#setwd("C:/Users/User/Dropbox/Data Science/Data Incubator Challenge")
setwd("C:/Users/wangjia/Desktop/Data Incubator Challenge")
######## download the zipfiles in 2015 from Amazon AWS
for(i in 1987:2016){
  for(j in 1:12){
        year <- i    
        month <- j
        if(year >= 2016 & i>8){
          print("There is no availale data")
        }else{
          zipFilename <- paste("On_Time_On_Time_Performance_", year, "_", month, ".zip", sep = "")
          fileURL <- paste("http://tsdata.bts.gov/PREZIP/", zipFilename, sep = "")
          fileDest <- paste("./",zipFilename, sep = "")
          print(paste("downloading", zipFilename))  # indicate which file is downloading
          download.file(fileURL, 
                        destfile = fileDest, 
                        method = "curl")
          unzip(zipFilename)    
        }
  }

}
dataDownloaded <- date()


######## read the data in 2016. Combine monthly data into a yearly dataset
data <- data.frame()
for(i in 1987:2016){
    for(j in 1:12){
        year <- i    
        month <- j
        if(year >= 2016 & i>8){
                  print("There is no availale data")
        }else{
                  fileName <- paste("On_Time_On_Time_Performance_", year, "_", month, ".csv", sep = "")
                  print(paste("Reading", fileName))  # indicate which data file is loading
                  monthlydata <- read.csv(fileName, sep = ",", header = T,
                                          colClasses = c( "TailNum" = "NULL", "OriginStateName"="NULL", "DestStateName" = "NULL", "Flights" = "NULL",                                          
                                                          "FirstDepTime"="NULL", "TotalAddGTime"="NULL", "LongestAddGTime"="NULL", "DivAirportLandings"="NULL", 
                                                          "DivReachedDest"="NULL", "DivActualElapsedTime"="NULL", "DivArrDelay"="NULL", "DivDistance"="NULL", 
                                                          "Div1Airport"="NULL", "Div1AirportID"="NULL", "Div1AirportSeqID"="NULL", "Div1WheelsOn"="NULL", 
                                                          "Div1TotalGTime"="NULL", "Div1LongestGTime"="NULL", "Div1WheelsOff"="NULL", "Div1TailNum"="NULL", 
                                                          "Div2Airport"="NULL", "Div2AirportID"="NULL", "Div2AirportSeqID"="NULL", "Div2WheelsOn"="NULL", 
                                                          "Div2TotalGTime"="NULL", "Div2LongestGTime"="NULL", "Div2WheelsOff"="NULL", "Div2TailNum"="NULL", 
                                                          "Div3Airport"="NULL", "Div3AirportID"="NULL", "Div3AirportSeqID"="NULL", "Div3WheelsOn"="NULL", 
                                                          "Div3TotalGTime"="NULL", "Div3LongestGTime"="NULL", "Div3WheelsOff"="NULL", "Div3TailNum"="NULL", 
                                                          "Div4Airport"="NULL", "Div4AirportID"="NULL", "Div4AirportSeqID"="NULL", "Div4WheelsOn"="NULL", 
                                                          "Div4TotalGTime"="NULL", "Div4LongestGTime"="NULL", "Div4WheelsOff"="NULL", "Div4TailNum"="NULL", 
                                                          "Div5Airport"="NULL", "Div5AirportID"="NULL", "Div5AirportSeqID"="NULL", "Div5WheelsOn"="NULL", 
                                                          "Div5TotalGTime"="NULL", "Div5LongestGTime"="NULL", "Div5WheelsOff"="NULL", "Div5TailNum"="NULL", 
                                                          "Quarter"="NULL", "UniqueCarrier"="NULL","AirlineID"="NULL",  "FlightNum"="NULL", "OriginAirportID"="NULL",
                                                          "OriginAirportSeqID"="NULL", "DestAirportID"="NULL", "DestAirportSeqID"="NULL", "DestCityMarketID"="NULL",
                                                          "DestStateFips"="NULL","DestWac"="NULL", "DepDelay" ="NULL", "DepDelayMinutes"="NULL", "DepDel15"="NULL",
                                                          "DepartureDelayGroups"="NULL", "DepTimeBlk"="NULL","TaxiOut"="NULL","WheelsOff"="NULL","WheelsOn"="NULL",
                                                          "TaxiIn"="NULL", "CRSArrTime"="NULL","ArrTime"="NULL","ArrivalDelayGroups"="NULL","ArrTimeBlk"="NULL", "DestState"="NULL",           
                                                          "CRSElapsedTime" ="NULL", "ActualElapsedTime"="NULL", "AirTime"="NULL", "Distance"="NULL", 
                                                          "DistanceGroup" ="NULL", "CarrierDelay"="NULL", "WeatherDelay" ="NULL", "NASDelay"="NULL", 
                                                          "SecurityDelay" ="NULL", "LateAircraftDelay" ="NULL", "X" = "NULL"))
                  data <- rbind(monthlydata, data)
                    }
            }
}
########################### Remove some nonrelated columns, and missing values #############################################

n <- dim(data)[1]
cancelledPercentage <- sum(data$Cancelled)/n # the overall precentage of cancelled flights
divertedPercentage <- sum(data$Diverted)/n   # the precentage of diverted flights
data <- data[which(data$Cancelled != 1), ] # delete the record which are cancelled 
data <- data[which(data$Diverted != 1), ]  # delete the record which are diverted 
Drop <- c("Cancelled","CancellationCode", "Diverted")           # delete columns related to cancellation and divertion
data <- data[, !(colnames(data) %in% Drop)] 
apply(data, 2, function(x) sum(is.na(x)))  # check if there are still missing values, and we find that there is no missing values any more

data[is.na(data)] = 0 # imupte delay cause data to 0 if there is no delay

delayVar <- data[,c("ArrDelayMinutes", "ArrDel15", "DepDelayMinutes", "DepDel15")]
summary(delayVar)
str(delayVar)
quantile(data$ArrDelayMinutes[which(data$ArrDelayMinutes > 0)], na.rm = FALSE)

################### Construct plotly graph for which airline has higher delay rate ######
library(plyr)
library(plotly)

######################### 1. delay frequency by Carrier and plot it  #################################################
arrDelayRate_airport <- aggregate(ArrDel15 ~ Carrier, FUN = mean, data=data, na.rm=F)
# calculate the average delayed length by carrier
avgArrDelayLength_airport <- aggregate(ArrDelayMinutes ~ Carrier, FUN = mean, data=data[which(data$ArrDel15>0),], na.rm=F)
c <- merge(arrDelayRate_airport, avgArrDelayLength_airport, by = "Carrier")

c$hoverArr <- with(c, paste("Delay rate: ", round(ArrDel15*100,2), "%", '<br>',
                            "Average delayed by ", round(ArrDelayMinutes,0), " minutes",sep=""))

p1 <- plot_ly(c, x = ~as.character(Carrier), y = ~ round(ArrDel15*100,2), type = 'bar', 
              name = 'Arrival delay',
              text = ~hoverArr) %>%
              layout(title = 'Delay rate by airline',
              xaxis = list(showgrid = FALSE, title = "Airline"),
              yaxis = list(showgrid = TRUE, title = "Delary rate in %"), 
              barmode = 'group')


################################## 2. delay frequency by Airport #########################
arrDelayRate_airport <- aggregate(ArrDel15 ~ DestCityName, FUN = mean, data=data, na.rm=F)
# calculate the average delayed length by airport
avgArrDelayLength_airport <- aggregate(ArrDelayMinutes ~ DestCityName, FUN = mean, data=data[which(data$ArrDel15>0),], na.rm=F)

a <- merge(arrDelayRate_airport, avgArrDelayLength_airport,  by="DestCityName")
a <- a[order(a$ArrDel15, decreasing = T),][1:10,]
a$hoverArr <- with(a, paste("Delay rate: ", round(ArrDel15*100,2), "%", '<br>',
                                                  "Average delayed by ", round(ArrDelayMinutes,0), " minutes",sep=""))

p2 <- plot_ly(a, x = ~as.character(DestCityName), y = ~ round(ArrDel15*100,2), type = 'bar', name = 'Depature delay',
              text = ~hoverArr) %>%
              layout(title = 'Top 10 airports (in city) with highest delay rate',
              xaxis = list(showgrid = FALSE, title = "Airport"),
              yaxis = list(showgrid = TRUE, title = "Delary rate in %"), 
              barmode = 'group')



############################## 3. Top 2 most frequent delay cities for each airline ###################################
arrDelayRate_airportCarrier <- aggregate(ArrDel15 ~ Carrier + DestCityName, FUN = mean, data=data, na.rm=F)
# calculate the average delayed length by carrier
avgArrDelayLength_airportCarrier <- aggregate(ArrDelayMinutes ~ Carrier + DestCityName, FUN = mean, data=data[which(data$ArrDel15>0),], na.rm=F)

ac <- merge(arrDelayRate_airportCarrier,avgArrDelayLength_airportCarrier, by = c("Carrier", "DestCityName"))
names(ac)[2:4] <- c('CityName', 'delayRate', 'avgDelayLength')
orderedEvent_ac <- ac[1:10,]

ac_CarrierTop <- data.frame()
for(i in levels(ac$Carrier)){
  ac_carrier <- ac[which(ac$Carrier == i), ]
  ac_CarrierA <- ac_carrier[order(ac_carrier$delayRate, decreasing = T),][1:2,]
  ac_CarrierTop <- rbind(ac_CarrierA, ac_CarrierTop)
}

ac_CarrierTop$hover <- with(ac_CarrierTop, paste("Delay rate: ", round(delayRate*100,2), "%", '<br>', 
                                                 "Average delayed by ", round(avgDelayLength,0), " minutes",sep=""))

p3 <- plot_ly(ac_CarrierTop, x = ~Carrier, y = ~CityName, text = ~hover, type = 'scatter', mode = 'markers',
              marker = list(size = ~round(delayRate*100,2), opacity = 0.5)) %>%
              layout(title = 'Delay frequency',
              xaxis = list(showgrid = F, title = "Airline"),
              yaxis = list(showgrid = F, title = ""))



###################################### 4. Delay Frequency over time #####################################
# Convert Depature time to a time format
library(lubridate)
x <- data$DepTime
time <- substr(as.POSIXct(sprintf("%04.0f", x), format='%H%M'), 12, 16)
data$departureTime <- time
data$departureHour <- hour(hms(as.character(data$departureTime)))

h_Carrier <- data.frame("departureHour" = c(1:max(data$departureHour)))
for(i in levels(data$Carrier)){
  data_carrier <- data[which(data$Carrier == i), ]
  arrDelayRate_hour <- aggregate(ArrDel15 ~ departureHour, FUN = mean, data=data_carrier, na.rm=F)
  # calculate the average delayed length by airport
  avgArrDelayLength_hour <- aggregate(ArrDelayMinutes ~ departureHour, FUN = mean, 
                                      data=data_carrier[which(data_carrier$ArrDel15>0),], na.rm=F)
  h <- merge(arrDelayRate_hour,avgArrDelayLength_hour, by="departureHour")
  names(h)[2:3] <- c(paste("delayRate",i,sep=""), paste("avgDelayLength",i, sep =""))
  h_Carrier <-  merge(h,h_Carrier, by = "departureHour")
}

p4 <- plot_ly(h_Carrier, x = ~departureHour) %>%
  add_trace(y = ~avgDelayLengthWN, name = 'WN', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthVX, name = 'VX', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthUA, name = 'UA', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthOO, name = 'OO', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthNK, name = 'NK', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthHA, name = 'HA', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthF9, name = 'F9', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthEV, name = 'EV', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthDL, name = 'DL', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthB6, name = 'B6', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthAS, name = 'AS', mode = 'lines') %>%
  add_trace(y = ~avgDelayLengthAA, name = 'AA', mode = 'lines') %>%
  layout(xaxis = list(title = "Hour"),
         yaxis = list (title = "Delayed length in minutes"))

p5 <- plot_ly(h_Carrier, x = ~departureHour) %>%
  add_trace(y = ~delayRateWN, name = 'WN', mode = 'lines') %>%
  add_trace(y = ~delayRateVX, name = 'VX', mode = 'lines') %>%
  add_trace(y = ~delayRateUA, name = 'UA', mode = 'lines') %>%
  add_trace(y = ~delayRateOO, name = 'OO', mode = 'lines') %>%
  add_trace(y = ~delayRateNK, name = 'NK', mode = 'lines') %>%
  add_trace(y = ~delayRateHA, name = 'HA', mode = 'lines') %>%
  add_trace(y = ~delayRateF9, name = 'F9', mode = 'lines') %>%
  add_trace(y = ~delayRateEV, name = 'EV', mode = 'lines') %>%
  add_trace(y = ~delayRateDL, name = 'DL', mode = 'lines') %>%
  add_trace(y = ~delayRateB6, name = 'B6', mode = 'lines') %>%
  add_trace(y = ~delayRateAS, name = 'AS', mode = 'lines') %>%
  add_trace(y = ~delayRateAA, name = 'AA', mode = 'lines') %>%
  layout(xaxis = list(title = "Hour"),
         yaxis = list (title = "Delayed frequency"))


## Plot it in one graph and publish on plotly.com

Sys.setenv("plotly_username"="yangying")
Sys.setenv("plotly_api_key"="s2ixmlqwhl")


pubp1 <- subplot(p1, p2, p3, 
                 nrows = 3, heights = c(0.3, 0.3, 0.4), widths = c(1), 
                 shareX = F, shareY = F, titleX = T, titleY = T)

pubp2 <- subplot(p4, p5,
                nrows = 2, heights = c(0.5, 0.5), widths = c(1), 
                shareX = T, shareY = F, titleX = T, titleY = T)

plotly_POST(pubp1, filename = "plot1")
plotly_POST(pubp2, filename = "plot2")