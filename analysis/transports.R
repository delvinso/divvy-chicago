# devtools::install_github("dkahle/ggmap")
library(tidyverse)
library(tidyquant)
library(viridis)
library(RPostgreSQL)
library(ggmap)
library(rgdal)
library(lubridate)
library(scales)
source("analysis/helpers.R")



# Connection to PostgreSQL ----

divvy_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "chicago-divvy-data", host = "localhost")

# function to send SQL queries and return results as a tibble
query <- function(sql, con = divvy_con) {
  # fetch(dbGetQuery(con, sql), n = 1e8) %>% 
  
  fetch(dbSendQuery(con, sql), n = 1e8) %>% 
    as_tibble()}

# daily bikes in transit ----
my_blue <- "#46BCDE"
daily_bikes <- query("SELECT * from daily_unique_bike_ids;")

daily_bikes %>%
  group_by(date) %>% 
  summarize(total_bikes = n()) %>% 
  ggplot(aes(x = date, y = total_bikes)) + 
  geom_line(size = 0.4, colour = my_blue) +
  scale_x_date("", date_breaks = "6 months", date_labels = c("%Y %b")) + 
  scale_y_continuous("Number of Bikes Used", breaks = seq(0, 4000, by = 1000), labels = scales::comma) + 
  ggtitle("Number of Unique Bikes Per Day") + 
  expand_limits(y = 0) +
  theme_dark_ds(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# monthly bikes in transit ----
monthly_bikes <- query("SELECT * from monthly_active_bikes;") 

png("output/monthly_bikes_used.png", width = 640, height = 480)
monthly_bikes %>% 
  ggplot(aes(x = date, y = bikes)) + 
  geom_line(size = 1.2, colour = my_blue) + 
  scale_x_date("", date_breaks = "6 months", date_labels = c("%Y %b")) + 
  scale_y_continuous("Number of Bikes Used", breaks = seq(0, 6000, by = 1000), labels = scales::comma) + 
  ggtitle("Chicago Divvy Unique Daily Bikes") +
  theme_dark_ds(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(caption = "delvinso.github.io") + 
  theme(plot.caption = element_text(size = 12))
dev.off()
# https://robots.thoughtbot.com/postgres-window-functions

# CREATE TABLE bike_station_ids AS
# SELECT
# id,
# bike_id,
# start_time,
# stop_time,
# end_station_id,
  
  # create a new variable called next_start_station_id which is the NEXT start_station_id from the window function?

# lead(start_station_id, 1) OVER w AS next_start_station_id,
# lead(start_time, 1) OVER w AS next_start_time
# FROM trips

  # creates a subquery which is grouped by bike_id, sorted by start_time
# WINDOW w AS (PARTITION BY bike_id ORDER BY start_time) 


# ORDER BY bike_id, start_time;
# DELETE FROM bike_station_ids WHERE next_start_station_id IS NULL;


bike_station_ids <- query("SELECT * FROM bike_station_ids")
bike_station_ids

station_agg <- query("SELECT * FROM station_aggregates")
station_agg
monthly_station_agg <- query("SELECT * FROM monthly_station_aggregates")
monthly_station_agg
hourly_station_agg <- query("SELECT * FROM hourly_station_aggregates")
hourly_station_agg

png("output/monthly_station_transports.png", width = 640, height = 480)
monthly_station_agg %>%
  group_by(month) %>% 
  summarize(perc_transported = (sum(transported_to_other_station)/ sum(total_drop_offs))) %>% 
  ggplot(aes(x = month, y = perc_transported)) +
  geom_line(size = 1.2, colour = my_blue) +
  scale_y_continuous("Percent of Bikes Transported", labels = scales::percent) + 
  scale_x_date("", date_breaks = "6 months", date_labels = c("%Y %b")) + 
  theme_dark_ds(base_size = 16) +
  expand_limits(y = 0) +
  ggtitle("Chicago Divvy Bikes Transported", "% of bikes manually moved after being dropped off") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(caption = "delvinso.github.io") + 
  theme(plot.caption = element_text(size = 12))
dev.off()

comm_hour_station <- query("SELECT
                             commarea,
                             hour,
                             SUM(transported_to_other_station)/ SUM(total_drop_offs) perc,
                             SUM(total_drop_offs) total
                           FROM hourly_station_aggregates h
                            INNER JOIN stations s
                            ON h.end_station_id = s.id
                           GROUP BY commarea, hour
                           HAVING SUM(total_drop_offs) > 500
                           ORDER BY commarea, hour
                           ")
# arrange by tpta;
comm_hour_station %>% arrange(desc(perc), total)

# most travelled community areas during rush hours
top_comm <- c("LOOP", "NEAR WEST SIDE", "NEAR NORTH SIDE", "LINCOLN PARK", "LAKE VIEW", "NEAR SOUTH SIDE"
              # "WEST TOWN"
              )

png("output/bike_transports_by_commarea.png", width = 640, height = 480)
comm_hour_station %>% 
  filter(commarea %in% top_comm) %>% 
  mutate(rush_hour = hour %in% c(6, 7, 8 ,9, 16, 17, 18, 19),
         timestamp_x = as.POSIXct(hour * 3600, origin = "1970-01-01", tz = "UTC")) %>%
  ggplot(aes(x = timestamp_x, y = perc)) + 
  geom_bar(aes(fill = commarea, colour = rush_hour), stat = "identity") +
  facet_wrap(~ commarea, nrow = 2, scales = "free_x") +
  scale_colour_manual(values = c(NA, "grey20")) + 
  scale_y_continuous("Percent of Bikes Transported", labels = scales::percent, breaks = seq(0, 0.3, 0.05)) + 
  scale_x_datetime("", date_breaks = "6 hours", date_labels = "%I %p") + 
  theme_dark_ds(base_size = 16) +
  ggtitle("Chicago Divvy Bikes Transported by Community Area", "% of bikes manually moved after being dropped off") + 
  guides(fill = FALSE, colour = FALSE) +
  labs(caption = "delvinso.github.io") + 
  theme(plot.caption = element_text(size = 12))
dev.off()
    