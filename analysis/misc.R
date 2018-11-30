

# Weather ----

aggregate_daily_weather <- query("SELECT * FROM daily_agg_weather")
aggregate_daily_weather <- dbGetQuery(divvy_con, "SELECT * FROM daily_agg_weather;") %>% as_tibble()
aggregate_daily_weather <- aggregate_daily_weather %>%
  mutate(weekday = dow %in% c(1:5),
                                   weekday_not_holiday =  dow %in% c(1:5) & !date %in% c('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25'))
glimpse(aggregate_daily_weather)
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# trips by temperature
# round to nearest whole number for aggregating purposes

aggregate_daily_weather %>%
  filter(weekday_not_holiday) %>%
  mutate(round_temp = round2(max_temp, 0),
         temperature_bucket = floor(max_temp / 5) * 5) %>%
  # group_by(round_temp) %>%
  group_by(temperature_bucket) %>%
  summarize(count = n(),
            avg_temp = mean(max_temp, na.rm = TRUE),
            avg_trips_by_temp = mean(trips, na.rm = TRUE),
            se_trips_by_temp = sd(trips, na.rm = TRUE) / sqrt(length(trips))) %>%
  ggplot(aes(x = avg_temp, y = avg_trips_by_temp)) +
  geom_line(colour = "lightgreen") +
  geom_point(alpha = 0.8, colour = "lightgreen", size = 1) +
  # geom_errorbar(aes(min = avg_trips_by_temp - se_trips_by_temp, ymax = avg_trips_by_temp + se_trips_by_temp), size = 0.5) +
  # geom_point(aes(y = trips), size = 1, alpha = 0.5) +
  theme_dark_ds(base_size = 16) +
  scale_y_continuous("Average # Trips", labels = scales::comma, breaks = seq(0, 15000, by = 5000), limits = c(0, 15000)) +
  scale_x_continuous("Temperature (°C)")

# trips by wind speed
aggregate_daily_weather %>%
  filter(weekday_not_holiday) %>%
  mutate(round_avg_wind_speed = round2(average_wind_speed, 0),
         wind_spd_bucket = floor(average_wind_speed/1.5) * 1.5) %>%
  # group_by(round_avg_wind_speed) %>%
  group_by(wind_spd_bucket) %>%
  summarize(count = n(),
            avg_wind_spd = mean(average_wind_speed, na.rm = TRUE),
            avg_trips = mean(trips, na.rm = TRUE),
            se_trips = sd(trips, na.rm = TRUE)/ sqrt(length(trips))) %>%
  ggplot(aes(x = avg_wind_spd, y = avg_trips)) +
  geom_line(colour = "lightgreen") +
  geom_point(alpha = 0.8, colour = "lightgreen", size = 1) +
  # geom_errorbar(aes(min = avg_trips - se_trips, ymax = avg_trips + se_trips), size = 0.1) +
  theme_dark_ds(base_size = 16)

# trips by snow depth - need to convert to inches?
quantile(aggregate_daily_weather$snow_depth_mm * 0.039, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.70, 0.75, 0.90, 0.99))

aggregate_daily_weather %>%
  filter(weekday_not_holiday) %>%
  mutate(snow_dep_in = snow_depth_mm * 0.039) %>%
  mutate(round_snow_dep = round2(snow_dep_in, 0)) %>%
  group_by(round_snow_dep) %>%
  summarize(count = n(),
            avg_trips = mean(trips, na.rm = TRUE),
            se_trips = sd(trips, na.rm = TRUE)/ sqrt(length(trips))) %>%
  ggplot(aes(x = round_snow_dep, y = avg_trips)) +
  geom_line(colour = "lightgreen") +
  geom_point(alpha = 0.8, colour = "lightgreen", size = 1) +
  # geom_errorbar(aes(min = avg_trips_by_temp - se_trips_by_temp, ymax = avg_trips_by_temp + se_trips_by_temp), size = 0.5) +
  theme_dark_ds(base_size = 16)

# trips by precipitation
quantile(aggregate_daily_weather$precip, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.70, 0.75, 0.90, 0.99))

aggregate_daily_weather %>%
  filter(weekday_not_holiday) %>%
  mutate(round_precip = round2(precip, 1),
         precip_buckets = cut(precip, c(0, 0.001, 0.01, 0.1, 0.2, 0.4, 1, 10), right = FALSE)) %>%
  group_by(precip_buckets) %>%
  summarize(count = n(),
            avg_precip = mean(precip, na.rm = TRUE),
            avg_trips = mean(trips, na.rm = TRUE),
            se_trips = sd(trips, na.rm = TRUE)/ sqrt(length(trips))) %>%
  na.omit() %>%
  ggplot(aes(x = avg_precip, y = avg_trips)) +
  geom_line(colour = "lightgreen") +
  geom_point(alpha = 0.8, colour = "lightgreen", size = 1) +
  # geom_errorbar(aes(min = avg_trips_by_temp - se_trips_by_temp, ymax = avg_trips_by_temp + se_trips_by_temp), size = 0.5) +
  theme_dark_ds(base_size = 16)

# trips by humidity

quantile(aggregate_daily_weather$relative_humidity, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99))

aggregate_daily_weather %>%
  filter(weekday_not_holiday) %>%
  mutate(round_humid = round2(relative_humidity, 1),
         humid_buckets = cut(relative_humidity, breaks = c(36.584, 50, 54.200, 66.700, 75, 83.300, 100), right = FALSE)) %>%
  group_by(humid_buckets) %>%
  summarize(count = n(),
            avg_humid = mean(relative_humidity, na.rm = TRUE),
            avg_trips = mean(trips, na.rm = TRUE),
            se_trips = sd(trips, na.rm = TRUE)/ sqrt(length(trips))) %>%
  ggplot(aes(x = avg_humid, y = avg_trips)) +
  geom_line(colour = "lightgreen") +
  geom_point(alpha = 0.8, colour = "lightgreen", size = 1) +
  # geom_errorbar(aes(min = avg_trips_by_temp - se_trips_by_temp, ymax = avg_trips_by_temp + se_trips_by_temp), size = 0.5) +
  theme_dark_ds(base_size = 16)

# humidex

 aggregate_daily_weather <-  aggregate_daily_weather %>%
  mutate(
          avg_wdspd_kmh = average_wind_speed * 3.6,
    e =  6.11 * exp(1)^( 5417.7530 * ( (1/273.16) - (1/dewp)) ),
    h = (5/9) * (e - 10.0)
         )%>%
  mutate(perceived_max_temp = ifelse(max_temp <= 0 & avg_wdspd_kmh  >= 5,
                             # equation 1 for wind chill - need to double check
                              13.12 + (0.6215 * max_temp) - (11.37 * avg_wdspd_kmh^0.16) + (0.3965 * max_temp * avg_wdspd_kmh^0.16),
                             # equation 2
                             # max_temp + ((-1.59 + 0.1345 * max_temp)/5) * average_wind_speed)
                             # humidex
                             ifelse(max_temp > 20 & dewp >= (10+273),
                                    max_temp + h,
                                  max_temp ))) #%>%
  # filter(max_temp <= 0) %>%
 aggregate_daily_weather %>% select(date, max_temp, avg_wdspd_kmh, e, h, dewp, perceived_max_temp) %>% arrange((dewp))
((1/273.16) - (1/(15+273.15)))
5417.7530 * ((1/273.16) - (1/(15+273.15)))
exp(1) ^( 5417.7530 * ((1/273.16) - (1/(15+273.15))) )
30 + 0.5555 * (6.11 * (exp(1) ^( 5417.7530 * ((1/273.16) - (1/(15+273.15)) ))) - 10)



# Trip Generation/Attraction Models ----


# multilevel modeling



# Can we predict the number of trips a station will have per station per weekday?

# Distributed Lag Model - serially correlated day to day selection behaviour, distributed lag models accunt for these serial correlations and temporary changes in station status and weather variation. In doing so, we make the
# assumption that day to day status of a station may affect future use. That is, if the station is damaged, has poor balancing, often relocated, it will lead to a decline in use. Similarly, if a station is in perfect condition
# it will result in future use.


# Weekday Station Level Generation Model
# Weekday Station Level



# understand the influence of factors in trip generation...


wday_sts_trips  <- query("SELECT
  date(start_time) AS date,
  EXTRACT(DOW from start_time) AS weekday,
  start_station_id,
  end_station_id,
  COUNT(*) AS trips,
  date(start_time) IN  ('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25') AS holiday
FROM trips
GROUP BY date, weekday, start_station_id, end_station_id;
")

daily_agg_weather <- query("SELECT * FROM daily_agg_weather;")

daily_agg_weather

# double check this - humidex and wind chill
daily_agg_weather <-  daily_agg_weather %>%
  mutate(
    avg_wdspd_kmh = average_wind_speed * 3.6,
    e =  6.11 * exp(1)^( 5417.7530 * ( (1/273.16) - (1/dewp)) ),
    h = (5/9) * (e - 10.0))%>%
  #wind-chill
  mutate(perceived_max_temp = ifelse(max_temp <= 0 & avg_wdspd_kmh  >= 5,
                                     # equation 1 for wind chill - need to double check
                                     13.12 + (0.6215 * max_temp) - (11.37 * avg_wdspd_kmh^0.16) + (0.3965 * max_temp * avg_wdspd_kmh^0.16),
                                     # equation 2
                                     ifelse(max_temp <0 & avg_wdspd_kmh >= 0 & avg_wdspd_kmh <= 5,
                                            max_temp + ((-1.59 + 0.1345 * max_temp)/5) * average_wind_speed,
                                            max_temp)
                                     )) %>%
  mutate(perceived_max_temp =  ifelse(max_temp >= 20 & dewp >= (10),
                                      # equation 2
                                      # max_temp + ((-1.59 + 0.1345 * max_temp)/5) * average_wind_speed)
                                      # humidex
                                            max_temp + h,
                                            max_temp )) #%>%
daily_agg_weather %>% ggplot(aes(x = max_temp, y = perceived_max_temp)) + geom_point()


#creating new factors based on weather

daily_agg_weather <- daily_agg_weather %>%
  mutate(freeze = max_temp < 0,
         cold_0_to_10 = max_temp >=0 & max_temp < 10,
         cool_10_to_20 = max_temp >= 10 & max_temp < 20 ,
         warm_20_to_30 = max_temp >= 20 & max_temp < 30) %>%
  mutate(year = year(date))



# generation or trips starting from a startion

wday_sts_gen <- wday_sts_trips %>%
  group_by(date, weekday, start_station_id) %>%
  summarize(total_trips = sum(trips)) %>%
  left_join(daily_agg_weather %>%
              select(-trips)) %>%
  # taking the log of trips to have it more approximately normally distributed
  mutate(log_total_trips = log10(total_trips), lag_log_total_trips = lag(log_total_trips, fill = NA)) %>%
  mutate(lag_total_trips = lag(total_trips, fill = NA)) %>%
  select(date, weekday, start_station_id, total_trips, lag_total_trips, lag_log_total_trips, everything())


wday_sts_gen

wday_sts_gen %>% ungroup() %>% count(hot)

# daily unique stations?

wday_sts_gen_lm <- lm(data = wday_sts_gen %>% filter(year == 2017), log_total_trips ~ lag_log_total_trips + freeze + cold_0_to_10 + cool_10_to_20 + warm_20_to_30 + precip + snow_depth_mm)
summary(wday_sts_gen_lm)
plot(wday_sts_gen_lm)
wday_sts_gen_lm

# multilevel modelling

# assess trip generation/attraction variation across day blocks
wday_sts_hourly <- query("SELECT * FROM weekday_sts_hourly;")

# weekday hourly trip generation
wday_sts_hour_gen <- wday_sts_hourly %>%
  # total trips by hour and START station
  group_by(date, hour, start_station_id) %>%
  summarize(total_trips = sum(trips)) %>%

  # create factorspertaining to day block

  mutate(day_block = case_when(
    hour >= 6 & hour < 10 ~ "peak_morn",
    hour >= 10 & hour < 14 ~ "midday",
    hour >= 14 & hour < 18 ~ "peak_aft",
    hour >= 18 & hour < 23 ~ "even",
    hour >= 23 & hour < 6 ~ "overnight"
  ))


# determine the total number of trips for a given days day block
wday_sts_hour_gen <- wday_sts_hour_gen %>%
  group_by(date, day_block, start_station_id, year) %>%
  summarize(total_trips = sum(total_trips))

test <- lme4::lmer(data = aggregate_daily_weather %>%
                     filter(weekday_not_holiday) %>%
                     mutate(freeze = perceived_max_temp < 0,
                            cold_0_to_10 = perceived_max_temp >=0 & perceived_max_temp < 10,
                            cool_10_to_20 = perceived_max_temp >= 10 & perceived_max_temp < 20 ,
                            warm_20_to_30 = perceived_max_temp >= 20
                            & perceived_max_temp < 30,
                            hot_30_plus = perceived_max_temp >= 30), log(trips) ~ (1|visibility) + (1|average_wind_speed) + (1|snow_depth_mm) +
                                (1|precip) + (1|relative_humidity) +
                                (1|freeze) +  (1|cold_0_to_10) + (1|cool_10_to_20) + (1|warm_20_to_30) + (1|hot_30_plus))

plot(test)
test
data.frame(lme4::VarCorr(test)) %>%
  mutate(total_var = sum(vcov),
                                          prop_var = vcov/total_var * 100) %>%
  as_tibble() %>%
  ggplot(aes(x = reorder(grp, prop_var), y = prop_var, fill = prop_var)) +
  geom_bar(stat = "identity") +
  coord_flip()

agglme4::ranef(test)
rand_test <- lmerTest::ranova(test)
rand_test







# OUTDATED ----

# the sf way  ----
library(sf)
areas <- st_read("data/chicago_ct_2010/chicago_ct_2010.shp", layer = "chicago_ct_2010") %>%
  st_transform( crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# CRS("+proj=latlong +datum=WGS84"))#+no_defs +nadgrids=@null +towgs84=0,0,0 +init=epsg:4326"))


wday_sf <- st_as_sf(weekday_sts_by_hour, coords =  c("start_lon", "start_lat"),
                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# mapping points to community areas
wday_sf_map <- st_join(areas, wday_sf)
# joining counts back to community area sf (lost during the join)
wday_sf_map2 <- areas %>% st_intersection(wday_sf_map)

wday_sf_map <- st_join(wday_sf, areas, join = st_intersects, left = FALSE)
data.frame(wday_sf_map$geometry)


ggplot() +
  # geom_sf(data = areas) +
  geom_sf(data = wday_sf_map2, aes(fill = counts))

# the sp way ----



# weekday station to station hourly counts to and from the loop (core dt, are there any other areas?)
weekday_sts_hour_morning <- query("
                                  WITH weekday_sts_hourly AS(
                                  SELECT date(start_time) AS date,
                                  EXTRACT(HOUR from start_time) AS hour,
                                  start_station_id,
                                  end_station_id,
                                  COUNT(*) AS trips
                                  FROM trips
                                  WHERE EXTRACT(DOW FROM start_time) BETWEEN 1 AND 5
                                  AND EXTRACT(HOUR from start_time) BETWEEN 6 and 9

                                  GROUP by date, hour, start_station_id, end_station_id
                                  )
                                  SELECT
                                  w.start_station_id,
                                  ss.latitude as start_lat,
                                  ss.longitude as start_lon,
                                  es.latitude as end_lat,
                                  es.longitude as end_lon,
                                  w.end_station_id,
                                  SUM(trips) AS total,
                                  COUNT(DISTINCT date) AS num_days
                                  FROM weekday_sts_hourly w,
                                  stations es,
                                  stations ss
                                  WHERE w.start_station_id = ss.id
                                  AND w.end_station_id = es.id
                                  GROUP by hour, start_lat, start_lon, end_lat, end_lon, start_station_id, end_station_id;")

weekday_sts_hour_night <- query("
                                WITH weekday_sts_hourly AS(
                                SELECT date(start_time) AS date,
                                EXTRACT(HOUR from start_time) AS hour,
                                start_station_id,
                                end_station_id,
                                COUNT(*) AS trips
                                FROM trips
                                WHERE EXTRACT(DOW FROM start_time) BETWEEN 1 AND 5
                                AND EXTRACT(HOUR from start_time) BETWEEN 16 and 19
                                GROUP by date, hour, start_station_id, end_station_id)
                                SELECT
                                w.start_station_id,
                                ss.latitude as start_lat,
                                ss.longitude as start_lon,
                                es.latitude as end_lat,
                                es.longitude as end_lon,
                                w.end_station_id,
                                SUM(trips) AS total,
                                COUNT(DISTINCT date) AS num_days
                                FROM weekday_sts_hourly w,
                                hour,
                                stations es,
                                stations ss
                                WHERE w.start_station_id = ss.id
                                AND w.end_station_id = es.id
                                GROUP by hour, start_lat, start_lon, end_lat, end_lon, start_station_id, end_station_id;")

wday_sts_morn_start <- query(" SELECT
                             w.start_station_id,
                             ss.latitude AS start_lat,
                             ss.longitude AS start_lon,
                             SUM(trips) AS total,
                             COUNT(DISTINCT date) AS num_days
                             FROM
                             weekday_sts_hourly w,
                             stations ss
                             WHERE w.start_station_id = ss.id
                             AND w.hour BETWEEN 6 AND 9
                             GROUP BY hour, start_lat, start_lon, start_station_id;")

test <- query("SELECT * FROM weekday_sts_hourly")
test %>% filter(start_station_id == 12, hour >= 6, hour <= 9 )
wday_sts_morn_end <- query(" SELECT
                           w.end_station_id,
                           es.latitude AS end_lat,
                           es.longitude AS end_lon,
                           SUM(trips) AS total,
                           COUNT(DISTINCT date) AS num_days
                           FROM
                           weekday_sts_hourly w,
                           stations es
                           WHERE w.end_station_id = es.id
                           AND w.hour BETWEEN 6 AND 9
                           GROUP By end_lat, end_lon, end_station_id")

wday_sts_night_start <- query(" SELECT
                              w.start_station_id,
                              ss.latitude AS start_lat,
                              ss.longitude AS start_lon,
                              SUM(trips) AS total,
                              COUNT(DISTINCT date) AS num_days
                              FROM
                              weekday_sts_hourly w,
                              stations ss
                              WHERE w.start_station_id = ss.id
                              AND w.hour BETWEEN 16 AND 19
                              GROUP BY start_lat, start_lon, start_station_id;")

wday_sts_morn_end <- query(" SELECT
                           w.end_station_id,
                           es.latitude AS end_lat,
                           es.longitude AS end_lon,
                           SUM(trips) AS total,
                           COUNT(DISTINCT date) AS num_days
                           FROM
                           weekday_sts_hourly w,
                           stations es
                           WHERE w.end_station_id = es.id
                           AND w.hour BETWEEN 6 AND 9
                           GROUP By end_lat, end_lon, end_station_id")


library(sp)
# reading in shapefile and transforming to WGGS84
areas <- spTransform(readOGR("data/community_area/chicomm2018.shp", layer = "chicomm2018"),
                     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# CRS("+proj=latlong +datum=WGS84"))#+no_defs +nadgrids=@null +towgs84=0,0,0 +init=epsg:4326"))

# creating ids
areas@data$id <- as.character(as.numeric(rownames(areas@data)) + 1)

# convert our data into a spatial points dataframe
sp_morning_start <- SpatialPointsDataFrame(coords = wday_sts_morn_start[, c("start_lon", "start_lat")], #normally lat lon? but because of WGS84DD..
                             data = wday_sts_morn_start,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
                             # proj4string = CRS("+proj=longlat +datum=WGS84"))

sp_morning_end <- SpatialPointsDataFrame(coords = wday_sts_morn_end[, c("end_lon", "end_lat")],
                                           data = wday_sts_morn_end,
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

sp_night_start <- SpatialPointsDataFrame(coords = wday_sts_night_start[, c("start_lon", "start_lat")], #normally lat lon? but because of WGS84DD..
                                           data = wday_sts_night_start,
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

sp_night_end <- SpatialPointsDataFrame(coords = wday_sts_night_end[, c("end_lon", "end_lat")],
                                         data = wday_sts_night_end,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#mapping the coordinates of stations into community area boundaries

sp_area_morning_start <- over(x = sp_morning_start, y = areas)
sp_area_morning_end <- over(x = sp_morning_end, y = areas)
sp_area_night_start <- over(x = sp_night_start, y = areas)
sp_area_night_end <- over(x = sp_night_end, y = areas)

# merging mapped data with area
sp_morning_start@data <- data.frame(sp_morning_start@data, sp_area_morning_start)
sp_morning_end@data <- data.frame(sp_morning_end@data, sp_area_morning_end)
sp_night_start@data <- data.frame(sp_night_start@data, sp_area_night_start)
sp_night_end@data <- data.frame(sp_night_end@data, sp_area_night_end)

# clean up
rm(sp_area_morning_start, sp_area_morning_end, sp_area_night_start, sp_area_night_end)

# aggregating station counts and joining with sp data, then joining to poly data

# early morning commute to work
am_start <- sp_morning_start@data %>%
  group_by(id) %>%
  summarize(counts = sum(total),
            total_days = sum(num_days),
            avg = counts/total_days) %>%
  left_join(areas@data, by = "id") %>%
  inner_join(broom::tidy(areas), by = "id") %>%
  ungroup() %>%
  select(counts:group) %>%
  distinct()

am_end <- sp_morning_end@data %>%
  group_by(id) %>%
  summarize(counts = sum(total),
            total_days = sum(num_days),
            avg = counts/total_days) %>%
  left_join(areas@data, by = "id") %>%
  inner_join(broom::tidy(areas), by = "id") %>%
  ungroup() %>%
  select(counts:group) %>%
  distinct()

pm_start <- sp_night_start@data %>%
  group_by(id) %>%
  summarize(counts = sum(total),
            total_days = sum(num_days),
            avg = counts/total_days) %>%
  left_join(areas@data, by = "id") %>%
  inner_join(broom::tidy(areas), by = "id") %>%
  ungroup() %>%
  select(counts:group) %>%
  distinct()

pm_end <- sp_night_end@data %>%
  group_by(id) %>%
  summarize(counts = sum(total),
            total_days = sum(num_days),
            avg = counts/total_days) %>%
  left_join(areas@data, by = "id") %>%
  inner_join(broom::tidy(areas), by = "id") %>%
  ungroup() %>%
  select(counts:group) %>%
  distinct()


ggplot(am_end, aes(x = long, y = lat)) +
  geom_polygon(data = broom::tidy(areas), aes(x = long, y = lat, group = group), colour = "white") +
  # geom_polygon(aes(x = long, y = lat, fill = avg, group = group)) +
  # stat_density_2d(aes(alpha = ..level.. * 1.5, fill = ..level..), geom = 'polygon', size = 0.15, show.legend = F, na.rm = TRUE) +
  geom_contour(aes( z=z)) +
  geom_path(aes(x = long,
                y = lat,
                group = group),
            color = "black", size = 0.1) +
  theme_dark_ds() +
  scale_fill_viridis() +
  theme(legend.position = "top") +
  coord_equal()

base_dens_map <- function(x, palette = "viridis"){
  # https://stackoverflow.com/questions/24198514/ggplot2-modify-geom-density2d-to-accept-weights-as-a-parameter

  kde2d.weighted <- function (x, y, w, h, n = 25, lims = c(range(x), range(y))) {
    nx <- length(x)
    if (length(y) != nx)
      stop("data vectors must be the same length")
    if (length(w) != nx & length(w) != 1)
      stop("weight vectors must be 1 or length of data")
    gx <- seq(lims[1], lims[2], length = n) # gridpoints x
    gy <- seq(lims[3], lims[4], length = n) # gridpoints y
    if (missing(h))
      h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y));
    if (missing(w))
      w <- numeric(nx)+1;
    h <- h/4
    ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
    ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
    z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2]) # z is the density
    return(list(x = gx, y = gy, z = z))
  }

  dens <- kde2d.weighted(x$long, x$lat, x$avg)
  dfdens <- data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z))


  ggplot(x, aes(x = long, y = lat)) +
    #draw entire map regardless of fill
    geom_polygon(data = broom::tidy(areas), aes(x = long, y = lat, group = group), colour = "#252525") +
    # draws contours weighted by avg
    stat_contour(data = dfdens, aes(x = x, y = y, z = z, fill = ..level..), geom = "contour") +
    # fills contour based on og data
    # stat_density_2d(aes(alpha = ..level.. * 1.5, fill = ..level..), geom = 'polygon', size = 0.15, show.legend = F, na.rm = TRUE) +
  geom_path(aes(x = long, y = lat, group = group), colour = "black", size = 0.1) +
  theme_dark_ds() +
  scale_fill_viridis(option = palette) +
  theme(legend.position = "top") +
  coord_equal()
}
base_dens_map(am_end)
base_dens_map(am_start)
base_dens_map(pm_start)
base_dens_map(pm_end)



cowplot::plot_grid(base_commarea_map(am_start),
          base_commarea_map(am_end))
base_commarea_map(am_start)



am_start_end <- bind_rows(am_start %>% mutate(type = "am_start"),
          am_end %>% mutate(type = "am_end")) %>%
  base_commarea_map() +
  facet_wrap(~ type)


pm_start_end <- bind_rows(pm_start %>% mutate(type = "pm_start"),
          pm_end %>% mutate(type = "pm_end")) %>%
  base_commarea_map(palette = "magma") +
  facet_wrap(~ type)

cowplot::plot_grid(am_start_end, pm_start_end, nrow = 2)

sextiles_am_start <- quantile(am_start$avg, probs = seq(0, 1, by = 1/6))

pretty_breaks <- c(seq(1 , 6, by = 1))

# Plotting each year's proportion of crime with discrete pretty breaks

am_start <- pretty_break_labeller(am_start, variable = "avg", pretty_breaks)
brks_scale <- levels(am_start$brks)
labels_scale <- rev(brks_scale)
base_area_map(am_start)
extendLegendWithExtremes(p, am_start$avg)



pretty_break_labeller <- function(x, variable, pretty_breaks){
  # find the extremes
  minVal <- min(x[[variable]], na.rm = T)
  maxVal <- max(x[[variable]], na.rm = T)
  # compute labels
  labels <- c()
  brks <- c(minVal, pretty_breaks, maxVal)
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }

  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  x$brks <- cut(x[[variable]],
                breaks = brks,
                include.lowest = TRUE,
                labels = labels)

  return(x)

}


extendLegendWithExtremes <- function(p, var){
  p_grob <- ggplotGrob(p)
  legend <- gtable::gtable_filter(p_grob, "guide-box")
  legend_grobs <- legend$grobs[[1]]$grobs[[1]]
  # grab the first key of legend
  legend_first_key <- gtable::gtable_filter(legend_grobs, "key-3-1-1")
  legend_first_key$widths <- unit(2, units = "cm")
  # modify its width and x properties to make it longer
  legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")

  # last key of legend
  legend_last_key <- gtable::gtable_filter(legend_grobs, "key-3-6-1")
  legend_last_key$widths <- unit(2, units = "cm")
  # analogous
  legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
  legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")

  # grab the last label so we can also shift its position
  legend_last_label <- gtable::gtable_filter(legend_grobs, "label-5-6")
  legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")

  # Insert new color legend back into the combined legend
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <-
    legend_first_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <-
    legend_last_key$grobs[[1]]
  legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <-
    legend_last_label$grobs[[1]]

  # finally, I need to create a new label for the minimum value
  new_first_label <- legend_last_label$grobs[[1]]
  new_first_label$label <- round(min(var, na.rm = T), 2)
  new_first_label$x <- unit(-0.15, units = "cm")
  new_first_label$hjust <- 1

  legend_grobs <- gtable::gtable_add_grob(legend_grobs,
                                  new_first_label,
                                  t = 6,
                                  l = 2,
                                  name = "label-5-0",
                                  clip = "off")
  legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
  p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend

  # the plot is now drawn using this grid function
  grid::grid.newpage()
  grid::grid.draw(p_grob)
}
