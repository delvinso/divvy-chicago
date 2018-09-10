# devtools::install_github("dkahle/ggmap")
library(tidyverse)
library(tidyquant)
library(viridis)
library(RPostgreSQL)
library(ggmap)
library(rgdal)
library(lubridate)
library(scales)
library(patchwork)
source("analysis/helpers.R")

# Connection to PostgreSQL ----

divvy_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "chicago-divvy-data", host = "localhost")

# function to send SQL queries and return results as a tibble
query <- function(sql, con = divvy_con) {
  # fetch(dbGetQuery(con, sql), n = 1e8) %>%

  fetch(dbSendQuery(con, sql), n = 1e8) %>%
    as_tibble()}


# Census Tracts & Community AreaMap Projection and Transformation  ----

      tracts <- spTransform(readOGR("data/chicago_ct_2010/chicago_ct_2010.shp", layer = "chicago_ct_2010"),
                            CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null +towgs84=0,0,0"))

      tracts@data$id <- as.character(as.numeric(rownames(tracts@data)) + 1)
      tracts_pts <- broom::tidy(tracts, region = "id")
      tracts_map <- inner_join(tracts_pts, tracts@data, by = "id")


      areas <- spTransform(readOGR("data/community_area/chicomm2018.shp", layer = "chicomm2018"),
                           # CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null"))
                           CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

      areas@data$id <- as.character(as.numeric(rownames(areas@data)) + 1)

      # converting spatial df into tabular data
      area_pts <- broom::tidy(areas, region = "id")
      # joining tabular data with shape data on id
      area_map <- inner_join(area_pts, areas@data, by = "id")

 # Heatmap of Community Areas Experiencing Greatest Overall Traffic ? (follow up to static heat map) ----

    # weekday station to station hourly counts to and from the loop (core dt, are there any other areas?)
    weekday_ctc_by_hour <- query("
                                 WITH weekday_sts_hourly AS(
                                   SELECT date(start_time) AS date,
                                   EXTRACT(HOUR from start_time) AS hour,
                                   start_station_id,
                                   end_station_id,
                                   COUNT(*) AS trips
                                   FROM trips
                                   WHERE EXTRACT(DOW FROM start_time) BETWEEN 1 AND 5
                                   GROUP by date, hour, start_station_id, end_station_id
                                 )
                                 SELECT
                                   w.hour,
                                   ss.commarea as start_commarea,
                                   es.commarea as end_commarea,
                                   SUM(trips) AS total,
                                   COUNT(DISTINCT date) AS num_days
                                FROM weekday_sts_hourly w,
                                   stations es,
                                   stations ss
                                 WHERE w.start_station_id = ss.id
                                   AND w.end_station_id = es.id
                                 GROUP by hour, start_commarea, end_commarea;")

    ctc_hourly_counts <- weekday_ctc_by_hour %>%
      # filter(start_commarea != end_commarea) %>% # would be interesting to see how many trips these make up
      group_by(hour, start_commarea, end_commarea) %>%
      mutate(avg = total/num_days) %>%
      arrange(desc(avg))

    quantile(ctc_hourly_counts$avg, probs = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) #top percentile is 13.742542

    ctc_hourly_counts  %>% arrange(desc(avg), start_commarea) %>% top_n(n = 20) %>% distinct(start_commarea)
    ctc_hourly_counts  %>% arrange(desc(avg), end_commarea)


  png(filename = "output/top_percentile_ctc_avg.png", width = 640, height = 840)

  ctc_hourly_counts  %>%
      mutate(timestamp_xaxis =  as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC")) %>%
      # filter on 1st percentile
      filter(avg >= 13.742542) %>%
      ggplot(aes(x = timestamp_xaxis, y = end_commarea)) +
        geom_tile(aes(fill = avg), alpha = 0.8, colour = "grey20") +
      # scale_fill_gradientn(colours = viridis(n = 20)[5:20], name = "Average Trips", na.value = NA) +
        scale_x_datetime("", date_breaks = "4 hours", labels = date_format("%l %p")) +
        scale_y_discrete("", position = "right") +
        facet_grid(start_commarea ~ ., switch = "y", labeller = labeller(groupwrap = label_wrap_gen(4))) +
        theme_bw(base_size = 14) +
        scale_fill_viridis(name = "Average Trips", na.value = NA) +
        ggtitle("Top Percentile of Average Hourly Trips from Community to Community ", "Chicago Divvy Bikeshare Data, 2013 - 2017") +
        theme_dark_ds() +
        theme_colbar() +
        labs(caption = "delvinso.github.io") +
        theme(plot.caption = element_text(size = 12))

  dev.off()



    # West Town ->
    # Near West Side ->
    # Near North Side ->
    # Loop
    # Logan Square
    # Lincoln Park ->
    # Lake View

  # community area traffic over time during weekdays
#
# library(gganimate)
#
#   weekday_ctc_by_hour_start <- query("
#                                 WITH weekday_sts_hourly AS(
#                                  SELECT date(start_time) AS date,
#                                    EXTRACT(HOUR from start_time) AS hour,
#                                    start_station_id,
#                                    end_station_id,
#                                    COUNT(*) AS trips
#                                  FROM trips
#                                  WHERE EXTRACT(DOW FROM start_time) BETWEEN 1 AND 5
#                                  GROUP by date, hour, start_station_id, end_station_id)
#                                SELECT
#                                  w.hour,
#                                  ss.commarea as start_commarea,
#                                  SUM(trips) AS total,
#                                  COUNT(DISTINCT date) AS num_days
#                                FROM weekday_sts_hourly w,
#                                  stations es,
#                                  stations ss
#                                WHERE w.start_station_id = ss.id
#                                AND w.end_station_id = es.id
#                                GROUP by hour, start_commarea")
#
#   ctc_hourly_start_map <- weekday_ctc_by_hour_start %>%
#     group_by(start_commarea) %>%
#     mutate(avg = total/num_days) %>%
#     left_join(area_map, by = c("start_commarea" = "community")) %>%
#     filter(hour %in% c(9, 10)) %>%
#     mutate(hour = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"))
#     # filter(start_commarea == "LOOP", hour == 6)
#
#
#    p <- ggplot(ctc_hourly_start_map ) +
#     geom_polygon(data = broom::tidy(areas), aes(x = long, y = lat, group = group), colour = "black") +
#     geom_polygon(aes(x = long, y = lat, fill = avg, group = group)) +
#     geom_path(aes(x = long, y = lat,  group = group),
#               color = "black", size = 0.1) +
#     scale_fill_viridis(option = "viridis", direction = 1,
#                        guide = guide_colorbar(
#                          direction = "horizontal",
#                          barheight = unit(2, units = "mm"),
#                          barwidth = unit(30, units = "mm"),
#                          draw.ulim = F,
#                          title.position = "top",
#                          title.hjust = 0.5,
#                          label.hjust = 0.5)) +
#     theme_dark_map() +
#     theme(legend.position = "top") +
#     ggtitle("Hour: ") +
#     transition_time(hour) +
#
#     # ease_aes('linear') +
#     coord_quickmap()
#
#   p
#
#   gganimate(p, filename = "output/ctc_hourly_wday_start_animation_50.gif", delay = 50)
#
#   ctc_hourly_end_map <- ctc_hourly_counts %>%
#     left_join(area_map, by = c("end_commarea" = "community"))
#   p <- ctc_hourly_end_map %>%
#     # filter(hour %in% c(6, 7, 8, 9)) %>%
#     ggplot() +
#     geom_polygon(data = broom::tidy(areas), aes(x = long, y = lat, group = group), colour = "#252525") +
#     geom_polygon(aes(x = long, y = lat, fill = avg, group = group, frame = hour)) +
#     geom_path(aes(x = long,
#                   y = lat,
#                   group = group),
#               color = "black", size = 0.1) +
#     scale_fill_viridis(option = "viridis", direction = 1,
#                        guide = guide_colorbar(
#                          direction = "horizontal",
#                          barheight = unit(2, units = "mm"),
#                          barwidth = unit(30, units = "mm"),
#                          draw.ulim = F,
#                          title.position = "top",
#                          title.hjust = 0.5,
#                          label.hjust = 0.5)) +
#     theme_dark_map() +
#     theme(legend.position = "top") +
#     ggtitle("Hour: ") +
#     coord_equal()





# Descriptive Statistics ----

subscriber_count <- query(" SELECT
                              user_type,
                              gender,
                              EXTRACT(YEAR FROM start_time) - birth_year::int AS age,
                              COUNT(*) AS count
                            FROM trips
                            WHERE user_type != 'Dependent'
                            GROUP BY user_type, gender, start_time, birth_year")
# break down by user_type
subscriber_count %>%
  group_by(user_type) %>%
  summarize(sum = sum(count)) %>%
  mutate(prop = prop.table(sum) * 100)

  # user_type by gender
subscriber_count %>%
  filter(age != "NA", !gender %in% c("", "NA")) %>%
  group_by(user_type, gender) %>%
  summarize(sum = sum(count)) %>%
  mutate(prop = prop.table(sum) * 100)

#median and mean age by gender
subscriber_count %>%
  filter(age != "NA", gender %in% c("Female", "Male")) %>%
  group_by(user_type, gender) %>%
  summarize(count = n(),
            mean_age = mean(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE))
#
# subscriber_count %>%
#   filter(user_type == "Subscriber", gender %in% c("Female", "Male")) %>%
#   mutate(prop = prop.table(count) * 100) %>%
#   ggplot(aes(x = user_type, y = count, fill = gender)) +
#   geom_bar(stat = "identity", position = "dodge2")

#  of the roughly 14 million trips made, 27.3% of them were from single-ride customers. Subscribers account for 72.3% of all trips made.
# Of these subscribers, there is a 3:1 male to female ratio.


# Monthly Rolling Sum - Trips -----

daily <- query("SELECT date(start_time) AS date,
               COUNT(*) AS trips
               FROM trips
               GROUP BY date(start_time) ")

daily_roll <- daily %>%
  tq_mutate(select = trips, mutate_fun = rollsum, k = 28, fill = NA, align = "right", col_rename = "monthlyTrips")
#
png(filename = "output/monthly_ridership.png", width = 640, height = 480)

daily_roll %>%
  ggplot(aes(x = date, y = monthlyTrips)) +
  geom_line(size = 1.2,
            colour = "#52D273"
            # colour = "lightgreen"
            ) +
  scale_x_date("", date_breaks = "4 months", labels = date_format("%Y %b")) +
  scale_y_continuous("Rolling 28 Day Counts", labels = scales::comma) +
  theme_dark_ds(base_size = 18) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Monthly Ridership", "Chicago Divvy Bikeshare Data, 2013 - 2017") +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))
dev.off()



# Hourly Trips, Weekdays vs Weekends ----

hour_by_dow <- query("SELECT
      EXTRACT(HOUR FROM t.start_time) AS hour,
      CASE
        WHEN EXTRACT(DOW FROM t.start_time) IN (1, 2, 3, 4, 5) THEN 'Weekdays'
        WHEN EXTRACT(DOW FROM t.start_time) IN (0, 6) THEN 'Weekends'
      END AS dow,
      COUNT(*) AS counts,
      COUNT(DISTINCT date(start_time)) AS num_days
    FROM trips t
    GROUP BY hour, dow
")

# initially grouped by year but trends were identical across years
# png(filename = "output/hour_by_dow.png", width = 640, height = 480)

hour_by_dow %>%
  mutate(timestamp_for_x_axis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"),
         avg = counts / num_days) %>%
  ggplot() +
    geom_bar(aes(x = timestamp_for_x_axis, y = avg, fill = avg), stat = "identity") +
  # , fill = "#21908CFF"
    facet_wrap(~ dow, nrow = 2, scales = "free_x") +
    scale_fill_distiller(palette = "Blues") +
    scale_y_continuous("Average Counts", labels = comma) +
    scale_x_datetime("", date_breaks = "4 hours", labels = date_format("%l %p")) +
    theme_dark_ds(base_size = 16) +
    guides(fill = FALSE) +
    ggtitle("Average # of Rides by Hour - Weekdays vs Weekends", "Chicago Divvy Bikeshare Data, 2013 - 2017")
# dev.off()



# There are peaks on weekdays corresponding to times where subscribers would be using the bikeshare system to get to work, and then to get home from work.
# We can break down the rides by user type to see if this is true.


user_hour_by_dow <- query("SELECT
      EXTRACT(HOUR FROM t.start_time) AS hour,
      t.user_type as user_type,
      CASE
        WHEN EXTRACT(DOW FROM t.start_time) IN (1, 2, 3, 4, 5) THEN 'Weekdays'
        WHEN EXTRACT(DOW FROM t.start_time) IN (0, 6) THEN 'Weekends'
      END AS dow,
      COUNT(*) AS counts,
      COUNT(DISTINCT date(start_time)) AS num_days
    FROM trips t
    GROUP BY hour, dow, user_type
")
# initially grouped by year but trends were identical across years
png(filename = "output/user_hour_by_dow.png", width = 640, height = 480)

user_hour_by_dow %>%
  filter(user_type != "Dependent") %>%
  mutate(timestamp_for_x_axis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC"),
         avg = counts / num_days) %>%
  ggplot() +
  # fill = "#21908CFF"
    geom_bar(aes(x = timestamp_for_x_axis, y = avg, fill = avg), stat = "identity") +
    facet_grid(user_type ~ dow) +
    scale_y_continuous("Overall Counts", labels = comma) +
    scale_x_datetime("", labels = date_format("%l %p")) +
    theme_dark_ds(base_size = 16) +
    scale_fill_distiller(palette = "Blues") +
    guides(fill = FALSE) +
    ggtitle("Average # of Rides by Hour - Weekdays vs Weekends", "Chicago Divvy Bikeshare Data, 2013 - 2017") +
    labs(caption = "delvinso.github.io") +
    theme(plot.caption = element_text(size = 12))

dev.off()

user_hour_by_dow %>%
  filter(user_type == "Subscriber" & dow == "Weekdays") %>%
  mutate(avg = counts / num_days) %>% print(n = 30)


# Distances ----

  # Visualizing distribution of counts and distance, all users and then by user_type
  # https://ropensci.org/blog/2017/10/17/bikedata/
  # calculating the distance required to make each station to station trip

# create subquery unique station counts by user type and join on legs
# joining station to station counts with distances
sts_dist_count <- query("WITH sts_by_user AS (
                        SELECT
                           start_station_id,
                           end_station_id,
                           user_type,
                           gender,
                           count(*) AS count
                        FROM trips
                        WHERE start_station_id != end_station_id
                          AND EXTRACT(HOUR FROM start_time) in (6, 7, 8, 9, 16, 17, 18, 19)
                          AND EXTRACT(DOW FROM start_time) in (1, 2, 3, 4, 5)
                          AND start_time NOT IN ('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25')
                        GROUP BY start_station_id, end_station_id, user_type, gender)
                    SELECT
                          SUM(DISTINCT(l.m)) AS total_m,
                          SUM(DISTINCT(l.minutes)) AS total_min,
                          l.start_station_id,
                          l.end_station_id,
                          s.count,
                          s.gender,
                          s.user_type
                    FROM legs l
                    INNER JOIN sts_by_user s
                        ON s.start_station_id = l.start_station_id
                        AND s.end_station_id = l.end_station_id
                    WHERE l.start_station_id != l.end_station_id
                          AND user_type != 'Dependent'
                    GROUP BY s.count, l.start_station_id, l.end_station_id, s.user_type, s.gender; ")

png(filename = "output/hex_rush_hour_dist_miles.png", width = 480, height = 480)
  sts_dist_count %>%
    filter(user_type == "Subscriber", gender %in% c("Female", "Male")) %>%
    mutate(dist_miles = total_m * (1/ (5280 * 12 * 2.54/100))) %>%
  ggplot( aes(x = dist_miles, y = count)) +
    # ggplot( aes(x = total_m/1000, y = count)) +
    stat_binhex(aes(fill = log(..count..)), alpha = 0.8) +
    scale_fill_gradientn(colours = magma(n = 10)[4:10], name = "log10 counts", na.value = NA) +
    # scale_fill_gradientn(colours = c("seagreen","goldenrod1"), name = "log10 counts", na.value = NA) +
    # scale_x_log10("Distance (km)", breaks = c(1, 3, 5, 10, 20, 40)) +
    scale_x_log10 ("Distance (miles)", breaks = c (0.1, 0.5, 1, 2, 5, 10, 20), # better breaks
                   labels = c ("0.1", "0.5", "1", "2", "5", "10", "20")) +
    scale_y_log10("Counts", breaks = c(1, 10, 100, 1000, 10000, 50000)) +
    geom_vline(aes(xintercept = 1.61), alpha = 0.8, linetype = "dotted", colour = "grey20", size = 1.2) +
    guides(fill = FALSE) +
    ggtitle("Distance vs. # of Trips", "Weekday Rush Hour (6am - 9am, 4pm - 7pm), 2013 - 2017") +
    theme_dark_ds(base_size = 16) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    # facet_wrap(~ gender) +
    labs(caption = "delvinso.github.io") +
    theme(plot.caption = element_text(size = 12))
dev.off()

# weighted means by user_type
sts_dist_count %>%
  ungroup() %>%
  filter(user_type == "Subscriber", gender %in% c("Female", "Male")) %>%
  mutate(dist_miles = total_m * (1/ (5280 * 12 * 2.54/100))) %>%
  group_by(user_type) %>%
  # summarize(wtd_mean = sum( (total_m / 1000) * as.vector(count) / sum(count), na.rm = TRUE))
summarize(wtd_mean = sum( dist_miles * as.vector(count) / sum(count), na.rm = TRUE))

# Customers on average, travel 3.07km whereas subscribers travel less


# Bike Speed by Demographics (Age, Gender) ----

  # How fast is a typical subscriber?
  # How do age and gender impact biking speed?
  # Are Google Maps cycling time estimates accurate?

  # Average Speed = distance travelled according to Google Maps / time
  # Should udnerstate the rider's actual average speed, as the trip does not account for factors such as time spent unlocking the bike, loading it back in, and various other distractions
  # More importantly, it assumes the rider followed the Google Maps directions to a T, if they took an alternate route, say longer, the distance travelled would be greater and the average trip speed
  # would have been underestimated. Conversely, if the rider knew of a shortcut, then the trip speed might be overestimated.
  # Todd showed that in NYC...

  # We don't know what a given rider's intent is, some may trying to get from one point to another as fast and quickly as possible, while some might be doing it for scenic/leisurely purposes that just so happened
  # to star at point A and then end at point B. These riders will most certainly not be following a direct route, and so they'll have a very slow average speed.

  # To analyze bike speed as accurately and meaningful as possible, we will have to restrict the analysis to trips who are most likely aiming to get from one point to another
    # Weekdays excluding holidays
    # Rush Hour (7am - 9am, 4 - 7pm) # When ridership peaks begin and end
    # Subscribers (as there is gender and age data)
    # Average Trip Speed


  rush_hour <- query("
  SELECT
        trip_duration AS actual_trip_duration,
        user_type,
        EXTRACT(YEAR FROM start_time) - birth_year::int AS age,
        start_station_id,
        end_station_id,
        gender
        FROM trips
        WHERE EXTRACT(HOUR FROM start_time) in (6, 7, 8, 9, 16, 17, 18, 19)
          AND EXTRACT(DOW FROM start_time) in (1, 2, 3, 4, 5)
          AND gender IS NOT NULL
          AND user_type = 'Subscriber'
          AND birth_year::int >= 1940
          AND birth_year::INT <= 2005
          AND start_station_id != end_station_id
          AND start_time NOT IN ('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25');") #maybe not 16?

  # joining trip data with expected durations on station id and user_type
  rush_hour_user <- rush_hour %>%
    left_join(
           sts_dist_count %>%
             select(- count) %>%
             # convert to seconds
             rename(gm_trip_dur = total_min) %>%
             mutate(gm_trip_dur = gm_trip_dur * 60), # remove unnecessary columns
           by = c("start_station_id", "end_station_id", "user_type", "gender")
          ) %>%
    # the difference between the actual and expected duration in seconds
    mutate(diff = (actual_trip_duration - gm_trip_dur))

  # conversions and cleaning
  rush_hour_user <- rush_hour_user %>%
    mutate(
           # converting expected durations to minutes
           # gm_trip_dur = gm_trip_dur,
           # calculating the difference between the trip and the expected duration
           dist_miles = total_m * (1/ (5280 * 12 * 2.54/100)),
           # converting meters into miles and calculating speed in mph
           trip_mph = dist_miles / (1 /(60 *60) * actual_trip_duration),
           gm_mph = dist_miles / (1 /(60 * 60) * gm_trip_dur)) %>%
    filter(gender %in% c("Male", "Female"))

  quantile(rush_hour_user$trip_mph, probs = c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99), na.rm = TRUE)
  quantile(rush_hour_user$gm_mph, probs = c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99), na.rm = TRUE)

  # aggregate line ----

    rush_hour_agg <- rush_hour_user %>%
      group_by(age, gender) %>%
      filter(trip_mph >= 2.788, trip_mph <= 15.3) %>%
      # summarize_at(vars(total_m, gm_trip_dur, diff, dist_miles, trip_mph, gm_mph),
      #              funs(mean = mean(., na.rm = TRUE),
      #                   med = median(., na.rm = TRUE),
      #                   count = n()))
      summarize(mean_diff = mean(diff),
                med_diff = median(diff),
                sd_diff = sd(diff),
                mean_mph = mean(trip_mph),
                med_mph = median(trip_mph),
                sd_mph = sd(trip_mph),
                count = n(),
                mean_exp = mean(gm_trip_dur),
                mean_age = mean(age),
                med_age = median(age))


    # distribution of counts to filter on low sample age by group combinations

    quantile(rush_hour_agg$count,probs = c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99) )

    # difference by age and gender
    rush_hour_agg %>%
      # filter on 90 percentile, arbitrary
      filter(count >= 257) %>%
      ggplot(aes(x = age, y = med_diff/60)) +
       geom_line(aes(colour = gender), size = 1.25) +
      theme_dark_ds()

    # actual speed by age and gender
    rush_hour_agg %>%
      # filter on 90 percentile, arbitrary
      filter(count >= 14259) %>%
      ggplot(aes(x = age, y = med_mph)) +
        geom_line(aes(colour = gender))

    # overall these graphs are noisy, binning age and distance into buckets would result in 'cleaner' results

  # hex graphs - overview ----
    # hex graphs - age vs difference between actual and expected trip duration according to google maps
      rush_hour_user %>%
        na.omit() %>%
        # bottom and top 1%
        filter(trip_mph > 3, trip_mph < 15) %>%
      ggplot( aes(x = age, y = (diff / 60))) +
        stat_binhex(aes(fill = log10(..count..))) +
        scale_fill_gradientn(colours = magma(n = 20)[20:1], name = "Frequency", na.value = NA) +
        scale_x_continuous(breaks = seq(10, 80, by = 10)) +
        # scale_y_log10() +
        guides(fill = FALSE) +
        ggtitle("Age vs. Difference between Actual and Expected Trip Time", "Chicago Divvy Bikeshare Data, 2013 - 2017") +
        theme_dark_ds(base_size = 16) +
        facet_wrap(~ gender, nrow = 2)

      # what is the outlier?
      rush_hour_user %>% filter( (diff/60) > 100) %>% arrange(desc(diff))

      # hex graphs : age vs speed
      rush_hour_user %>%
        na.omit() %>%
        # bottom and top 1%
        filter(trip_mph > 3, trip_mph < 15) %>%
        ggplot( aes(x = age, y = trip_mph)) +
        stat_binhex(aes(fill = log10(..count..))) +
        scale_fill_gradientn(colours = inferno(n = 20)[20:1], name = "Frequency", na.value = NA) +
        scale_x_continuous(breaks = seq(10, 80, by = 10)) +
        # scale_y_log10() +
        guides(fill = FALSE) +
        facet_wrap(~ gender, nrow = 2) +
        ggtitle("Age vs. Speed (mph)", "Chicago Divvy Bikeshare Data, 2013 - 2017") +
        theme_dark_ds(base_size = 16)


  # bucket line graphs ----

    # use quantiles to determine appropriate buckets?
    quantile(rush_hour_user$dist_miles, probs = seq(0, 1, by = 1/4), na.rm = TRUE)
    quantile(rush_hour_user$gm_trip_dur, probs = seq(0, 1, by = 1/4), na.rm = TRUE)

    # creating buckets based on age, distance and expected durations
    rush_hour_user <- rush_hour_user %>%
      # na.omit() %>%
      mutate(age_bucket = cut(age, breaks = c(0, 18, 22, 25, 30, 35, 40, 45, 50, 60, 100), right = FALSE),
             dist_bucket = cut(dist_miles, breaks = c(0, 1, 1.5, 2.5, 15), right = FALSE),
             expected_bucket = cut(gm_trip_dur, breaks =c(0, 300, 600, 900, 1200, 5000), right = FALSE))
    # assigning proper levels to buckets
    levels(rush_hour_user$dist_bucket) <- c("< 1 miles", "1 to 1.5 miles", "1.5 to 2.5 miles", "≥2.5 miles" )
    levels(rush_hour_user$expected_bucket) <-  paste("Exp", c("<5", "5-10", "10-15", "15-20", "≥ 25"), "minutes")

    # summary statistics by distance
    rush_hour_user %>%
      na.omit() %>%
      group_by(dist_bucket) %>%
      summarize(mean_diff = mean(diff)/60,
                sd_diff = sd(diff)/60,
                mean_mph = mean(trip_mph),
                sd_mph = sd(trip_mph))
    # find the top and bottom percentiles of speed (ie. the extremes)
    quantile(rush_hour_user$trip_mph, c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99), na.rm = TRUE)

    rush_hour_user %>%
      filter(trip_mph > 2.788, trip_mph < 15.306) %>%
      ggplot() +
      # bottom and top 1%
      geom_histogram(aes(x = trip_mph, fill = ..count..))

    # summary statistics for buckets
    rush_hour_buckets <- rush_hour_user %>%
      na.omit() %>%
      # filtering on 1st and 99th percentiles
      filter(trip_mph > 3, trip_mph < 15) %>%

      group_by(dist_bucket, age_bucket, gender) %>%
      summarize(mean_diff = mean(diff),
                med_diff = median(diff),
                sd_diff = sd(diff),
                mean_mph = mean(trip_mph),
                med_mph = median(trip_mph),
                sd_mph = sd(trip_mph),
                count = n(),
                mean_exp = mean(gm_trip_dur),
                mean_age = mean(age),
                med_age = median(age))

    # gathering median and mean statistics
    rush_hour_buckets <- rush_hour_buckets %>%
      # gather(key = age_stats, value = age, c(mean_age, med_age)) %>%
      gather(key = speed_stats, value = speed, c(mean_mph, med_mph, sd_mph)) %>%
      gather(key = diff_stats, value = diff, c(mean_diff, med_diff, sd_diff))

    quantile(rush_hour_buckets$count, probs = c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99))

# speed by age and gender

png(filename = "output/age_vs_speed_bucket_by_dist.png", width = 640, height = 640)

rush_hour_buckets  %>%
  # greater than 90th percentile of counts for better accuracy
  filter(count >= 580.25) %>%
  # sd is roughly the same at ~2 mph across distance buckets, age, and gender
  filter(speed_stats != "sd_mph") %>%
ggplot( aes(x = mean_age, y = speed, colour = gender, linetype = speed_stats)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point() +
  facet_wrap(~ dist_bucket, nrow = 2) +
  theme_dark_ds(base_size = 16) +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"),
        legend.key.size = unit(1, "mm"),
        legend.direction = "horizontal") +
  guides(linetype = guide_legend(nrow = 2, byrow = TRUE, title = ""),
         colour = guide_legend(nrow = 2, byrow = TRUE, title = "")) +
  labs(x = "Age", y = "Speed (mph)") +
  ggtitle("Average Speed by Gender and Binned Distances",
          "Chicago Divvy Bikeshare, Weekday Rush Hour (6am - 9am, 4pm - 7pm), 2013 - 2017") +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))
dev.off()

# Overall, men are faster on average by..
rush_hour_buckets %>% group_by(gender, age_bucket) %>% summarize(mean_speed = mean(speed))
rush_hour_buckets %>% group_by(gender) %>% summarize(mean_speed = mean(speed), n= n())
rush_hour_buckets %>% ungroup() %>% summarize(mean_speed = mean(speed), n= n())

# mean actual time vs expected by age and gender, 0 indicates no difference from the actual time
png(filename = "output/age_vs_diff_bucket_by_dist.png", width = 640, height = 640)

rush_hour_buckets  %>%
  filter(diff_stats != "sd_diff") %>%
  filter(count >= 590) %>%
  ggplot( aes(x = mean_age, y = diff / 60, colour = gender, linetype = diff_stats)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point() +
  expand_limits(y = 0) +
  facet_wrap(~ dist_bucket, nrow = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = c("Mean", "Median")) +

  theme_dark_ds(base_size = 16) +
  theme(legend.position = "top",
        legend.key.width = unit(2, "cm"),
        legend.key.size = unit(1, "mm"),
        legend.direction = "horizontal") +
  guides(linetype = guide_legend(nrow = 2, byrow = TRUE, title = ""),
         colour = guide_legend(nrow = 2, byrow = TRUE, title = "")) +
  labs(x = "Age", y = "Average trip time - Expected trip time (min)") +
  ggtitle("Actual vs. Expected Duration of Trip by Gender and Binned Distances",
          "Chicago Divvy Bikeshare, Weekday Rush Hour (6am - 9am, 4pm - 7pm), 2013 - 2017") +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))

dev.off()

rush_hour_buckets  %>% group_by(gender, age_bucket) %>% summarize(mean_diff = mean(diff)/60, n = n())
rush_hour_buckets  %>% group_by(gender, dist_bucket) %>% summarize(mean_diff = mean(diff)/60, n = n())

rush_hour_buckets %>% group_by(gender) %>% summarize(mean_diff = mean(diff)/60)
rush_hour_buckets %>% ungroup %>% summarize(mean_diff = mean(diff)/60)


  # granular ----

  age_mean <- rush_hour_user %>%
    group_by(age, gender) %>%
    summarize(count = n(), mean_diff = mean(diff, na.rm = TRUE))

  quantile(age_mean$count, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99))

  age_mean %>%
    filter(count >= 2233, age >= 20, age <= 60) %>%
    ggplot() +
      geom_line(aes(x = age, y = mean_diff / 60, colour = gender)) +
      theme_dark_ds(base_size = 14) +
      theme(legend.title = element_blank(),
            legend.position = c(0.975, 0.925),
            legend.justification = "right")

  speed_mean <- rush_hour_user %>%
    filter(gender != "") %>%
    group_by(age, gender) %>%
    summarize(count = n(),
              mean_speed = mean(trip_mph, na.rm = TRUE))

  quantile(speed_mean$count, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99))

  speed_mean %>%
    filter(count >= 2233, age >= 20, age <= 60) %>%
  ggplot() +
    geom_line(aes(x = age, y = mean_speed, colour = gender)) +
    theme_dark_ds(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = c(0.975, 0.925),
          legend.justification = "right")

  distance_mean <- rush_hour_user %>%
    filter(gender != "") %>%
    mutate(nearest_dist_m = round(total_m, 1)) %>%
    group_by(nearest_dist_m, gender) %>%
    summarize(count = n(),
              mean_dist = mean(nearest_dist_m, na.rm = TRUE),
              mean_diff = mean(diff, na.rm = TRUE),
              mean_speed = mean(trip_mph, na.rm = TRUE))

  quantile(distance_mean$count, probs =  c(0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 0.90, 0.99))

  distance_mean %>%
    filter(count >= 4) %>%
    ggplot() +
    geom_line(aes(x = mean_dist, y = mean_diff/60, colour = gender))

  distance_mean %>%
    filter(mean_speed <= 15.32) %>%
    filter(count >= 4) %>%
    ggplot() +
    geom_line(aes(x = mean_dist, y = mean_speed, colour = gender), alpha = 0.5)

# Anonymity  ----
# For each hour, we count the number of trips made from a given station to an end station for a subscriber. We also know their age and gender.
  # for rounding to nearest hour, by default date_trunc rounds downwards
  # also birth year is varchar, need to change this
  # https://stackoverflow.com/questions/6195439/postgres-how-do-you-round-a-timestamp-up-or-down-to-the-nearest-minute
  anon_hourly <- query("SELECT * FROM anon_hourly;")

  anon_hour_identifiable <- anon_hourly %>%
    group_by(gender, age) %>%
    mutate(identifiable = ifelse(count == 1, 1, 0)) %>%
    summarize(total = n(),
              fraction = sum(identifiable)/total) %>%
    filter(gender != "")
  # clean up
  rm(anon_hourly)

  # on average, what percent can we identify someone based on their ride information
  anon_hour_identifiable %>% ungroup() %>% summarize(mean(fraction))
  # on average, what percent can we identify someone based on their ride information for each gender
  anon_hour_identifiable %>%
    group_by(gender) %>%
    summarize(mean(fraction)) %>%
    filter(gender != "")


png("output/anonymous_hourly.png", width = 480, height = 480)
ggplot(anon_hour_identifiable) +
  geom_line(aes(x = age, y = fraction, colour = gender), size = 1.5, alpha = 0.75) +
  geom_hline(aes(yintercept = 0.95), linetype = "dashed", size = 1, alpha = 0.7) +
  # facet_wrap(~ gender, nrow = 2) +
  scale_y_continuous("Percent of Uniquely Identifiable Subscribers", labels = scales::percent_format(), limits = c(0.9, 1)) +
  scale_x_continuous("Age") +
  theme_dark_ds(base_size = 16) +
  theme(legend.position = "bottom") +
  ggtitle("Uniquely Identifiable Divvy Bike Trips", "Given the rider's age, start and end point rounded to nearest hour ") +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))
dev.off()

anon_daily <- query("SELECT * FROM anon_daily;")

anon_daily_identifiable <- anon_daily %>%
  group_by(gender, age) %>%
  mutate(identifiable = ifelse(count == 1, 1, 0)) %>%
  summarize(total = n(),
            fraction = sum(identifiable)/total) %>%
  filter(gender != "")

  #cleanup
  rm(anon_daily)

png("output/anonymous_daily.png", width = 480, height = 480)

ggplot(anon_daily_identifiable ) +
  geom_line(aes(x = age, y = fraction, colour = gender), size = 2, alpha = 0.8) +
  geom_hline(aes(yintercept = 0.95), linetype = "dashed", size = 0.5, alpha = 0.5) +
  # facet_wrap(~ gender, nrow = 2) +
  scale_y_continuous("Percent of Uniquely Identifiable Subscribers", labels = scales::percent_format(), limits = c(0.5, 1)) +
  scale_x_continuous("Age") +
  theme_dark_ds(base_size = 16) +
  theme(legend.position = "bottom") +
  ggtitle("Uniquely Identifiable Divvy Bike Trips", "Given the rider's age, start and end point rounded to nearest day ") +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))

dev.off()
# clean up
rm(anon_daily, anon_hourly)





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



## choloropeth of morning and night start and end community areas ----


wday_morn_start <- query("
      SELECT
        ss.commarea AS start_commarea,
        SUM(trips) AS total,
        COUNT(DISTINCT date) AS num_days
      FROM
        weekday_sts_hourly w,
        stations ss
      WHERE w.start_station_id = ss.id
        AND hour BETWEEN 6 AND 9
      GROUP BY start_commarea")

wday_morn_start %>% mutate(avg = total/num_days) %>% arrange(desc(avg))

wday_morn_end <- query("
                       SELECT
                        es.commarea AS end_commarea,
                        SUM(trips) AS total,
                        COUNT(DISTINCT date) AS num_days
                       FROM
                        weekday_sts_hourly w,
                        stations es
                       WHERE w.end_station_id = es.id
                        AND hour BETWEEN 6 AND 9
                       GROUP BY end_commarea")
wday_morn_end %>% mutate(avg = total/num_days) %>% arrange(desc(avg))


wday_night_start <- query("SELECT
                            ss.commarea AS start_commarea,
                            SUM(trips) AS total,
                            COUNT(DISTINCT date) AS num_days
                          FROM
                            weekday_sts_hourly w,
                            stations ss
                          WHERE w.start_station_id = ss.id
                            AND hour BETWEEN 16 and 19
                          GROUP BY start_commarea")

wday_night_start %>% mutate(avg = total/num_days) %>% arrange(desc(avg))


wday_night_end <- query("SELECT
                            es.commarea AS end_commarea,
                          SUM(trips) AS total,
                          COUNT(DISTINCT date) AS num_days
                          FROM
                          weekday_sts_hourly w,
                          stations es
                          WHERE w.end_station_id = es.id
                          AND hour BETWEEN 16 and 19
                          GROUP BY end_commarea")

wday_night_end %>% mutate(avg = total/num_days) %>% arrange(desc(avg))

# bind_rows(wday_morn_start %>% mutate(type = "morn") %>% rename(community = "start_commarea"),
#           wday_morn_end %>% mutate(type = "night") %>%  rename(community = "end_commarea")) %>%
#   group_by(type, community) %>%
#   mutate(avg = total/num_days) %>%
#   inner_join(area_map, by = "community") %>%
#   base_commarea_map() +
#   facet_wrap(~ type)
#
# bind_rows(wday_morn_start %>% mutate(type = "morn") %>% rename(community = "start_commarea"),
#           wday_morn_end %>% mutate(type = "night") %>%  rename(community = "end_commarea")) %>%
#   group_by(type, community) %>%
#   mutate(avg = total/num_days) %>%
#   base_commarea_map() +
#   facet_wrap(~ type)

area_map %>%
  inner_join(wday_morn_start, by = c("community" = "start_commarea")) %>%
  group_by(community) %>%
  mutate(avg = total/num_days ) %>%
  base_commarea_map() +
area_map %>%
  inner_join(wday_morn_end, by = c("community" = "end_commarea")) %>%
  group_by(community) %>%
  mutate(avg = total/num_days ) %>%
  base_commarea_map(palette = "inferno") +
  plot_layout(nrow = 1)



area_map %>%
  inner_join(wday_night_start, by = c("community" = "start_commarea")) %>%
  group_by(community) %>%
  mutate(avg = total/num_days ) %>%
  base_commarea_map(palette = "inferno") +

area_map %>%
  inner_join(wday_night_end, by = c("community" = "end_commarea")) %>%
  group_by(community) %>%
  mutate(avg = total/num_days ) %>%
  base_commarea_map(palette = "inferno") +
  plot_layout(nrow = 1)




# heatmap of morning and night start/end community areas ----

wday_morn_ctc <- query("SELECT
                         hour,
                         ss.commarea AS start_commarea,
                         es.commarea AS end_commarea,
                         SUM(trips) AS total,
                         COUNT(DISTINCT date) AS num_days
                       FROM weekday_sts_hourly w,
                       stations es,
                       stations ss
                       WHERE w.start_station_id = ss.id
                        AND w.end_station_id = es.id
                        AND hour BETWEEN 6 and 9
                       GROUP BY hour, start_commarea, end_commarea")

wday_night_ctc <- query("SELECT
                         hour,
                         ss.commarea AS start_commarea,
                         es.commarea AS end_commarea,
                         SUM(trips) AS total,
                         COUNT(DISTINCT date) AS num_days
                       FROM weekday_sts_hourly w,
                       stations es,
                       stations ss
                       WHERE w.start_station_id = ss.id
                       AND w.end_station_id = es.id
                       AND hour BETWEEN 16 and 19
                       GROUP BY hour, start_commarea, end_commarea")

wday_morn_ctc <- wday_morn_ctc %>%
  mutate(avg = total/num_days) %>%
  arrange(desc(avg)) #%>%

wday_night_ctc <- wday_night_ctc %>%
  mutate(avg = total/num_days) %>%
  arrange(desc(avg)) #%>%

quantile(wday_morn_ctc$avg, probs = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) #top 1% is 16.462
quantile(wday_night_ctc$avg, probs = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) # top 1% is 23.06

png(filename = "output/wday_morn_rush_ctc.png", width = 480, height = 640 )
wday_morn_ctc %>%
  mutate(timestamp_xaxis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC")) %>%
  filter(avg >= 16.642) %>%
  arrange(desc(avg)) %>%
  ggplot(aes(x = timestamp_xaxis, y = end_commarea)) +
    geom_tile(aes(fill = avg), colour = "grey20", alpha = 0.8) +
    scale_fill_viridis(option = "viridis", name = "Average Trips") +
    scale_x_datetime("", date_breaks = "1 hour", labels = date_format("%l %p")) +
    scale_y_discrete("", position = "right") +
    facet_grid(start_commarea ~ ., switch = "y") +
    theme_bw(base_size = 14) +
  theme_dark_ds() +
  ggtitle("Top Percentile of Average Hourly Trips from Community to Community", "Morning Rush Hour (6am - 9am), 2013 - 2017") +
  theme_colbar() +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))
dev.off()

png(filename = "output/wday_night_rush_ctc.png", width = 480, height = 640)
wday_night_ctc %>%
  mutate(timestamp_xaxis = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC")) %>%
  filter(avg >= 16.642) %>%
  arrange(desc(avg)) %>%
  ggplot(aes(x = timestamp_xaxis, y = end_commarea)) +
  geom_tile(aes(fill = avg), colour = "grey20", alpha = 0.8) +
  scale_fill_viridis(option = "viridis", name = "Average Trips") +
  scale_x_datetime("", date_breaks = "1 hour", labels = date_format("%l %p")) +
  scale_y_discrete("", position = "right") +
  facet_grid(start_commarea ~ ., switch = "y") +
  theme_bw(base_size = 14) +
  theme_dark_ds() +
  ggtitle("Top Percentile of Average Hourly Trips from Community to Community", "Night Rush Hours (4pm - 7pm), 2013 - 2017") +
  theme_colbar() +
  labs(caption = "delvinso.github.io") +
  theme(plot.caption = element_text(size = 12))


dev.off()












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
