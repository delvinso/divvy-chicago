---
title: "14 Million Bike Rides in The Windy City (2013 - 2017) - An Analysis of Chicago’s Bikeshare System"
author: "Delvin So"
date: '2018-06-30'
output:
    html_document:
      theme: journal
      df_print: paged
      toc: true
      toc_float: true
      keep_md: true
      mathjax: default
      self_contained: false
      lib_dir: libs
      code_folding: hide
      number_sections: true
editor_options:
  chunk_output_type: console
---


## Pre-Face

I came across Todd Schneider's posts on [NYC taxi and uber trips](LINK) and subsequently another post on [Citi Bike](http://toddwschneider.com/posts/a-tale-of-twenty-two-million-citi-bikes-analyzing-the-nyc-bike-share-system/). He made his [code available](https://github.com/toddwschneider/nyc-citibike-data) and as an aspiring data analyst who also uses R, I took the opportunity to do a similar analysis with Chicago's Divvy Bikeshare data with modifications of my own.

The analysis is broken down into several categories.

##### Future Directions

* Impact of weather on trips made through modeling
  + I want to experiment on a smaller dataset before applying models here, maybe Toronto's bikeshare? **IN PROGRESS**.
* ~~Spatial heat-map of starting and ending trips made during morning and evening peak hours, respectively?~~
* Relationship between Income & Trip rate
* Rebalancing (https://medium.com/@Urbica.co/city-bike-rebalanced-92ac61a867c7)

## Overview

Divvy is a ride-sharing service offered in Chicago, and approximately 14 million trips were taken from 2013 - 2017 with the data being made [publicly available by Divvy](https://www.divvybikes.com/system-data).

27.3% of all trips made were from single-ride customers and 72.7% were from subscribers. Of the subscribers, there are 3 males for every 1 female rider, whose average age is 36 and 34 years old, respectively.

The high ratio of subscriber to single rider customers suggests that a majority of users use the bike-share program to commute.

We can take all 14 million trips made since the inception of Divvy up until 2017 and assuming everyone followed Google Maps directions (which does not really hold), we can create a static heat map to visualize which paths were travelled the most often across the 4 years. Below, each dot represents a station, and each trip consisting of paths between the start and stop station where the bike was picked up and docked, respectively. Thicker, brighter lines indicate paths that were more commonly used and thus travelled.

<!-- ![Caption for the picture.](output/divvy_hires_log10_sz_05_5_qm_newest_legdb.png) -->
<!-- ![Caption for the picture.](divvy_routes_heatmap_stdres.png) -->

<img src="output/divvy_hires_log10_sz_05_5_qm_newest_legdb.png" alt="drawing" width="475.2px"/>

The most popular areas are located near downtown, and the near north/west/east sides of the city.

I then took the top percentile of average hourly trips made from community area to community area to further visualize which areas experienced the greatest overall traffic.

![Caption for the picture.](output/top_percentile_ctc_avg.png)


The community area on the left (the facet name) indicates the starting community area, with the community areas on the right indicating the ending area. Empty tiles indicate no data available (ie. no trips made during that time period). The Loop (downtown) and the Near West Side experience the greatest traffic during peak rush hours (8am and 5pm) as expected, due to those areas having the greatest density of offices.

If you're interested, there are additional heatmaps and a chloropleth based on peak ridership (subscribers only) during rush hour by community area [LINK](.gdgdfgd)

## The Data and Questions

The data for each trip in the system is anonymized (has it really though? we will explore this later) and is as follows:

* Timestamps corresponding to trip start and end time
* Trip start and end station
* Rider type - Subscriber or Customer
* If the trip corresponds to a subscribers trip, it will also include their gender and year of birth
* Bike ID

The current dataset includes data from 2013 to 2017, which I have combined with weather data from GSOD. The two member types as mentioned are subscribers, who have 45 minutes/day, and customers, who have 30mins/ride. More information on the member types can be found [here](https://www.divvybikes.com/pricing).

Due to the size of the dataset (~approx 14 million trips), a database in postgreSQL was created to store the data. Queries and all downstream processing and analyses were performed in R.

The companion code/notebook for importing and cleaning the data can be in [my github repo](delvinso.github.io).

So now we know what the data looks like, what are some questions that we have regarding the data?


* Who uses the bikes? (Demographics)
  * More men or women?
  * Older or younger?
  * Subscribers or one time users?
* Where are bikes being checked out and riden (?)
  * Uptown vs Downtown? Commercial? Residential? Tourist? Leisure?
* When are the bikes being checked out?
  * Weekdays vs Weekends Weekend vs Holidays?
  * Rush Hour?
  * Seasonal?
* Why are the bikes being taken out?
  * Recreation vs Commuting?
    * Loop to Other Areas and Vice-Versa
  * Touristic Purposes?
  * Bypassing traffic?
* How?
  * How does the user demographics affect the duration the bikes are being used? Or where they are being checked out?
  * How does weather and/or traffic conditions impact bike usage?
  * How do the characteristics of the static location affect the # of bikes being checked out?

We have, for the most part answered the 'who' and the 'where', let's continue to the when.

## Ridership

![Caption for the picture.](output/monthly_ridership.png)

Nothing too crazy here, peak ridership occurs during the summer months, dipping down towards winter. Peaks and valleys have increased, albeit slowly. I wonder if the increase in the valleys are due to increased ridership (ie. more customers, so more people riding during the winter seasons) and/or if global warming plays a factor as well.

Next, we can visualize the ridership by user type (ie. subscriber or customer), by weekday on an hourly basis.

![Caption for the picture.](output/user_hour_by_dow.png)

As expected, customers take more trips during the weekends, and subscribers take more trips during the weekdays. The peaks for subscribers during weekdays at 6-8am and 4-6pm correspond with commutes to and from work, respectively. A possible reason for the late night afternoon peak being greater than the morning peak is that traffic and/or subway traffic is worse during the late afternoon relative to the morning, leading to more people opting to use bikeshare to commute home.

Future Questions
* How has the average by hour changed by year?

## (Approximate) Distance Travelled

How far does the typical rider travel on the bikeshare? We can't literally answer this as the trip data doesn't include the  distance travelled, but we do know the start and end location of each trip. We can use the start and end coordinates of each possible station to station combination (> 100 000 combinations), and query Google Maps Directions API to retrieve 'optimal' routes which include

* the distance of the station to station trip
* the duration of the station to station trip


This allows us to perform some guesswork on the distance travelled, assuming that all riders **intend to go from point A to B** and **are pragamatic (ie. they choose the safest and shortest route )** . (This is how the static heatmap was generated).

Which riders fit this criteria? Those who are commuting to and from work! We can assume trips made under the following conditions are by riders who are commuting to and from work:


* Weekdays excluding holidays
* Rush Hour (6am - 9am, 4 - 7pm), when ridership peaks begin and end
* Subscribers (as there is gender and age data)

With these limitations in mind, we can't assess customer trips (ie. one use trips)..




The graph depicts the number of log trips for binned log distances (in km) travelled. Lighter colours indicate lower number of trips made for a particular binned distance, whereas darker colours indicate higher number of trips.

![Caption for the picture.](output/hex_rush_hour_dist_miles.png?)

The graph indicates that the number of trips exponentially decrease with increasing distance, and that most trips are under ~7km or ~4.3miles. We can calculate the average distance as a weighted mean, which is indicated by the dotted line at 2.59km or 1.61miles.


## Average Speed Among Member-Types

Similarly to the distance travelled, we can't actually calculate the actual speed of a trip, but we do know the duration of a trip and the estimated distance (given by Google Maps directions API per above). This allows us to calculate the speed, and in addition to knowing the age and gender of subscribers, allows us to determine the answer to the following questions:



1. How accurate are Google Maps cycling directions?

2. What is the effect of age and gender on speed?

This will undoubtedly understate the rider's actual average speed, since the trip time doesn't account for factors such as time spent unloading and locking the bike, checking directions, traffic for that particular day, the weather conditions (ie. wind, rain, etc) and so on. We're also assuming that the rider followed the Google Maps direction to a T, when likely the rider takes their own personal route which likely deviates from Google Maps directions. This could be either longer, which will understate the rider's actual average speed, or be shorter, which will overstate the rider's actual average speed. Last of all, the purpose of the trip is completely lost as data - the rider may be simply trying to get to work and thus to their destination as soon as possible, or they could be taking a leisurely ride across Chicago.

To analyze bike speed as accurately, and as meaningfully as possible, we will restrict the analysis to trips which are most likely aiming to get from one point to another as fast as possible. A prime subset of trips would be the trips commuting to and from work seen in the previous section.

* Weekdays excluding holidays
* Rush Hour (7am - 9am, 4pm - 7pm), when ridership peaks begin and end
* Subscribers (as there is gender and age data)
* Average Trip Speed between the top and bottom percentile



The data was bucketed into distance travelled, age, and gender and the average speed (in mph) was calculated.


1. How accurate are Google maps cycling directions?

![Caption for the picture.](output/age_vs_diff_bucket_by_dist.png)

A difference of 0 would mean that there is no difference between the average duration of a trip and the expected duration. A positive difference indicates that the actual duration of a trip was greater than the expected duration, meaning trips are longer than google map estimates. Again, this is likely due to time spent docking and undocking the bike, traffic conditions, time spent lounging around, and so forth. Across all bucketed distances, there is an average positive difference if 10 minutes, with the difference being greater for females than males. Secondly, the difference decreases with age and is more evident with increasing trip distances.

* Indicate 0 using a dashed-line


2. What is the effect of age and gender on speed?

![Caption for the picture.](output/age_vs_speed_bucket_by_dist.png)


The average speed is 6.24 mph, with the average speed of male subscribers at 6.52mph and female subscribers at 5.96mph. We can see that on average, speed decreases with age, males ride faster than females, and as distance increases so does speed.

* Quantify the effect of speed, distance and age?


## Anonymity

Given the nature of a bikeshare, there are no precise coordinates regarding the users destination as all trips must start and end at a bike station. However, the trip data does contain demographic information regarding subscribers, such as their gender and birth year. In addition, if we know the date and time they picked up the bike rounded to the nearest hour, we are able to uniquely identify the individual trip 97.2% of the time. This means one could found out where and when that particular user dropped off their bike.

Todd's code used date_trunc, which simply truncates to the hour rather than rounding. To be more accurate, I modified the function so that it would round to the nearest hour only if above the half hour threshold, and down if below.

![Caption for the picture.](output/anonymous_hourly.png)

 Given the 3:1 male to female ridership, the rate should be even higher for females, and it in fact is, at 98.3% and 96.2% respectively. Compared to the analysis of NYC's Citibike, the percentage is much higher (77% and 92% for male and females, respectively), likely due to a smaller ridership for Divvy at the time of analysis and logistics behind the rounding of the hour.

## The Bike Balancing Act - In Progress

Bike availability at a given station is a major issue, especially during rush hour. If a particular station, let's say outside an office building or subway station is packed to the rim with bikes, but riders take them to other stations, that leaves that particular station with no more bikes. Due to commuting patterns as we saw above, bikes tend to aggregate in downtown areas during the early morning, draining the number of available bikes from, for example, the near west side. Thus, a large part in managing any bikeshare is rebalancing - ensuring that there are enough bikes at a given station at anytime.


![](output/bike_transports_by_commarea.png)

The Loop and the Near West Side, two of the most active community areas in terms of bike share use experience the most rebalancing. **EXPAND**
* Investigate how these rates have changed over years. According to [this article](https://dssg.uchicago.edu/project/predicting-when-divvy-bike-share-stations-will-be-empty-or-full/), a partnership between Divvy and the Department of Transportation in Chicago has attempted to use a poisson model to aid them in rebalancing. Has this been useful?

![](output/monthly_station_transports.png)


## Multilevel Modeling - In Progress

To investigate the effect that environmental factors have on the number of trips, we can use multilevel models. Weather data was collected from GSOD using the Chicago O'Hare weather station data, which is located x km from bike share stations. The data includes various information (LINK), notably temperature, wind...

To better understand how temperature affects rides,sub-zero temperatures and above-zero temperatures to wind chill and humidex values, respectively.  Wind chill combines the effect of wind speed and measured temperature, whereas humidex values combine the effects of humidity and measured temperatures. These perceived temperatures are a stronger indicator of what an individual feels while biking relative to solely the temperature, and is more likely to impact the number of trips made. http://climate.weather.gc.ca/glossary_e.html


 The natural logarithm of trips was used as the response variable.
We can fit a multilevel model with trips as the response to examine the variation that factors have on the trips...



### Skills Developed & Lessons Learned

* Working with SQL, notably postgreSQL and postGIS spatial calculations, database integration with R using RPostgreSQL
* Working with geospatial data, namely the sp package
* Generating direction/leg data of all possible pairwise trips using using Google Maps Directions API through the ggmap package
* Further honing coding skills in R, specifically using ggplot2 and tidyverse code
* With larger datasets, it's more efficient to do queries/subqueries and calculations within postgresql (which I technically SHOULD do with the anonymity section as it takes a while to query the data..)

## Concluding Remarks

With any dataset, there are always more questions that could be explored. In the future, I would like to model ridership as a function of environmental conditions, for example weather and econometric measures. The effect that weather has is interesting and has been visited before, but in the grand scheme of things it would have to be combined with other variables to be meaningful. As for econometric measures, one is more likely to use a bike sharing system if the station is close to their home or if the station is close to a university, but at the same time it wouldn't make economic sense for the company to construct a station at every block. Therefore a possible model to explore would be to determine what effect the proximity of these constructs has on ridership alone, and possibly with weather data. Another possibility would be to combine the bikeshare data with taxi data to determine how one affects thee other, but this would be more difficult as Uber and Lyft is [reluctant](http://activetrans.org/blog/ride-hailing-data-transparency-long-overdue) to release their data in Chicago.

Again, I would like to credit Todd for sharing his analysis on NYC Citibike system.

## Additional Reading

Gebhart, K., & Noland, R. B. (2014). The impact of weather conditions on bikeshare trips in Washington, DC. Transportation, 41(6), 1205–1225. https://doi.org/10.1007/s11116-014-9540-7

Fishman  Washington, S., Haworth, N., E. (2013). Bike Share: A Synthesis of the Literature. Transport Reviews, 33(2), 148–165. https://doi.org/10.1080/01441647.2013.775612

Romanillos, G., Zaltz Austwick, M., Ettema, D., & De Kruijf, J. (2016). Big Data and Cycling. Transport Reviews, 36(1), 114–133. https://doi.org/10.1080/01441647.2015.1084067
