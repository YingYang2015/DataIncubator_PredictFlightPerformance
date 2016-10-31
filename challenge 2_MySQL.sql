use dataincubatorchallenge;

select * from citibike;
decimal(5,2)
###############################What fraction of rides start and end at the same station###############################
select n, m, n/m as fraction 
from (select count(*) as n
		from citibike
		where `start.station.id` = `end.station.id`) as Tn, 
	(select count(*) as m
	from citibike) as Tm;

###############################what is the standard deviation of the number of stations visitited by a bike###############################
# I consider visiting a station multiple times as just visit one station.

select std(num_station)
from (select count(distinct station) as num_station
	  from (select `start.station.id` as station, bikeid from citibike c1
			union all
			select `end.station.id` as station, bikeid from citibike c2) as T1
	  group by bikeid) as T2;
      
      
########################## What is the differenece between the longest and shortest average duration? ###################      
select max(avgTripduration) - min(avgTripduration)
from (select avg(tripduration) as avgTripduration
	  from citibike
	  group by month(STR_TO_DATE(starttime, '%m/%d/%Y %H:%i'))) as T;
      

########################## What fraction of rides exceed their corresponding time limit?  ######################################
select n, m, n/m as fraction
from (select count(*) as m from citibike) as Tm, 
	 (select count(*) as n from citibike
	  where (usertype = 'customer' and tripduration > 30*60) or
		    (usertype = 'subscriber' and tripduration > 45*60)) as Tn;
             
#######################################largest ratio of station hourly usage fraction to system hourly usage fraction############################
select max(fraction) from(
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
    on T5.hstarttime = T6.hstarttime
) as T7