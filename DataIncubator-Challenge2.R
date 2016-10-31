#' This script is for data incubator challenge (2nd)
#' In this question, I automatically scraped monthly data in 2015 from the web
#' I stored the data in the database management system, mySql, because it is faster to perform 
#' the computation, and more secure. Therefore, most of the questions are answered by mySQL quries. 
#' 
#' Date: 10/31/2016
#' Author: Ying Yang



#######################transferring data from AWS to mySQL local database#########################

#setwd("C:/Users/User/Dropbox/Data Science/Data Incubator Challenge")
setwd("C:/Users/wangjia/Desktop/Data Incubator Challenge")
######## download the zipfiles in 2015 from Amazon AWS
for(i in 1:12){
        zipFilename <- ifelse(i < 10,
                              paste("20150",i,"-citibike-tripdata.zip", sep = ""),        
                              paste("2015",i,"-citibike-tripdata.zip", sep = "")
        )
        fileURL <- paste("https://s3.amazonaws.com/tripdata/", zipFilename, sep = "")
        fileDest <- paste("./",zipFilename, sep = "")
        print(paste("downloading", zipFilename))  # indicate which file is downloading
        download.file(fileURL, 
                      destfile = fileDest, 
                      method = "curl")
        unzip(zipFilename)
}
dataDownloaded <- date()

######## read the data in 2015. Combine monthly data into a yearly dataset
data_2015 <- data.frame()
for(i in (1:12)){
        fileName <- ifelse(i < 10,
                           paste("20150",i,"-citibike-tripdata.csv", sep = ""),        
                           paste("2015",i,"-citibike-tripdata.csv", sep = "")
        )
        print(paste("Reading", fileName))  # indicate which data file is loading
        monthlydata <- read.csv(fileName, sep = ",", header = T, 
                                colClasses = c("start.station.name"="NULL", "end.station.name" ="NULL",
                                               "birth.year"="NULL", "gender"="NULL", "stoptime" = "NULL" ))
        data_2015 <- rbind(monthlydata, data_2015)
}

dim(data_2015)
data_2015 <- data_2015[which(data_2015$tripduration <= 24*60*60),]
dim(data_2015)  


######## quick check if the data has any missing values or obvious input mistakes. I did not find any
sum(is.null(data_2015)) 	
sum(data$tripduration <= 0)


######## Link R with MySQL database
library(RMySQL)
library(psych)
library(DBI)
### Connect to the local database
# mydb <- dbConnect(MySQL(),
#                   user="root", host="localhost", port=3306, password = "19850719@yy")

mydb <- dbConnect(MySQL(),
                  user="root", host="localhost", port=3306, password = "data")


dbSendQuery(mydb, "CREATE DATABASE DataIncubatorChallenge;")
dbGetQuery(mydb, "use DataIncubatorChallenge;")

#### Transfer the existing Data to MySQL database
dbWriteTable(mydb, "citibike", data_2015, overwrite = TRUE)

#' set the digits requirement
options(digits=10)

###############################What fraction of rides start and end at the same station###############################
tripDuration <- dbGetQuery(mydb, "select tripduration
                       from citibike;")

median(as.numeric(tripDuration))
###############################What fraction of rides start and end at the same station###############################
fraction <- dbGetQuery(mydb, "select m, n, n/m as fraction
                       from (select count(*) as n
                       from citibike
                       where `start.station.id` = `end.station.id`) as Tn, 
                       (select count(*) as m
                       from citibike) as Tm;")
###############################what is the standard deviation of the number of stations visitited by a bike###############################
# I consider visiting a station multiple times as just visit one station.

stdStation <- dbGetQuery(mydb, "select std(num_station)
                        from (select count(distinct station) as num_station
                        from (select `start.station.id` as station, bikeid from citibike c1
                        union all
                        select `end.station.id` as station, bikeid from citibike c2) as T1
                        group by bikeid) as T2;")

########################## What is the differenece between the longest and shortest average duration? ###################
diffDuration <- dbGetQuery(mydb, "select max(avgTripduration) - min(avgTripduration)
                          from (select avg(tripduration) as avgTripduration
                      	  from citibike
                      	  group by month(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i'))) as T;
                           ")
########################## What fraction of rides exceed their corresponding time limit?  ######################################
dbGetQuery(mydb, "select n, m, n/m as fraction
                  from (select count(*) as m from citibike) as Tm, 
	                (select count(*) as n from citibike
	                where (usertype = 'customer' and tripduration > 30*60) or
		              (usertype = 'subscriber' and tripduration > 45*60)) as Tn;")

#######################################largest ratio of station hourly usage fraction to system hourly usage fraction############################
dbGetQuery(mydb, "select max fraction from(
          select T5.hstarttime, ratio1, ratio2, ratio1/ratio2 as fraction
           from
           (select T2.ssid, T2.hstarttime, T2.count, T1.countSum, T2.count/T1.countSum as ratio1
           from 
           (select `start.station.id` as ssid, count(*) as countSum
           from citibike
           group by `start.station.id`) as T1 
           join
           (select `start.station.id` as ssid, hour(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i:%s')) as hstarttime, count(*) as count
           from citibike
           group by `start.station.id`, hour(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i:%s'))) as T2
           on T1.ssid = T2.ssid) as T5
           join
           (select T3.hstarttime, T3.count, T4.countSum, T3.count/T4.countSum  as ratio2
           from     
           (select hour(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i:%s')) as hstarttime, count(*) as count
           from citibike
           group by hstarttime) as T3
           join 
           (select hour(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i:%s')) as hstarttime, count(*) as countSum 
           from citibike) as T4
           on T4.hstarttime = T3.hstarttime) as T6
           on T5.hstarttime = T6.hstarttime) as T7;")
dbDisconnect(mydb)



####################### average number of times a bike is moved in 2015################################## 

## define a function to find out if bike is ever picked up at a different station

movedBikeFun <- function(subsetData){
  n <- dim(subsetData)[1]
  flag <- replicate(0, n-1)
  for(i in 1:n-1){
    for (j in (i+1:n)){
      if(subsetData$bikeid[j] == subsetData$bikeid[i]){
        if(subsetData$start.station.id[j]!=subsetData$end.station.id[i]){
          flag[i] <- 1 # if the bike is picked up at a different station
          break
        }else{
          flag[i] <- 0 # if the bike is picked up at the same station
          break
        }
      }else{
        flag[i] <- 0     # if the bike is not picked up again in 2015
        break
      }
      
    }
  }
  return(flag)
}

#' this is to show if the bike is picked up at a different station in each ride for every bike
movedBikeVec <- data.frame()
for(i in levels(as.factor(data_2015$bikeid))){
  subsetData <- data_2015[which(data_2015$bikeid == i),]
  movedBike[,i] <- movedBikeFun(subsetData)
}
avg(sum(movedBike)) # this is the average number of times a bike is moved during this period

