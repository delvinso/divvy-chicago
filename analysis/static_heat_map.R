library(tidyverse)
library(tidyquant)
library(viridis)
library(RPostgreSQL)
library(ggmap)
library(rgdal)
library(lubridate)
library(scales)
library(sp)
source("analysis/helpers.R")

## CREATE AN INDEX
# This R script retrieves the leg data stored in 'legs' and joins it with the unique station to station counts in 'sts_uniq' to

# Connection to PostgreSQL ----

divvy_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "chicago-divvy-data", host = "localhost")

# function to send SQL queries and return results as a tibble
query <- function(sql, con = divvy_con) {
  # fetch(dbGetQuery(con, sql), n = 1e8) %>%

  fetch(dbSendQuery(con, sql), n = 1e8) %>%
    as_tibble()
  }


# Most Popular Routes  ----

  stations <- query("SELECT id, external_id, name, latitude, longitude, chict2010_gid, commid, commarea FROM stations")

  # reading in the legs, note this takes a bit of time as there are over 28 million rows
  # joining unique station to station counts with leg data for static heat map
  res_counts <- query("
                      WITH sts_uniq AS (
                      SELECT
                        start_station_id, end_station_id, count(*) AS count,
                        COUNT(DISTINCT(date(start_time))) as num_days
                      FROM trips
                      WHERE start_station_id != end_station_id
                      GROUP BY start_station_id, end_station_id
                      )
                      SELECT
                        l.start_lon,
                        l.end_lon,
                        l.start_lat,
                        l.end_lat,
                        st.count,
                        l.m as distance,
                        st.num_days,
                        st.start_station_id,
                        st.end_station_id,
                        l.minutes
                      FROM sts_uniq st
                      INNER JOIN legs l
                        ON st.start_station_id = l.start_station_id
                        AND st.end_station_id = l.end_station_id
                      WHERE st.start_station_id != st.end_station_id;")

  res_counts <- res_counts %>%
    # rename(start_station_id = start_id, end_station_id = end_id) %>%
    # create groups so ggplot knows how the legs should be connected
    mutate(grp = (paste(start_station_id, end_station_id, sep = "."))) %>%
    arrange(count)

  # http://boundingbox.klokantech.com

  min_lat <- 41.716999
  max_lat <- 42.081900
  min_lon <- -87.827344
  max_lon <-  -87.532917
  btm = min_lat
  top = max_lat
  left = min_lon
  right = max_lon

  

  # Standard Res  ----
  start_time <- Sys.time()
  png(filename = "output/divvy_routes_heatmap_stdres2.png", width = 960, height = 1600, bg = "black")

  ggplot() +
    # geom_path(data = area_map, aes(x = long, y = lat, group =))
    geom_leg(data = res_counts, aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat,
                                    colour = count, alpha = count, size = count, group = grp)) +
    geom_point(data = stations,
               aes(x = longitude, y = latitude),
               color = "#F05123", alpha = 0.7, size = 1.5) +
    scale_size_continuous(range = c(0.3, 3)) + #0.15 # 3 is looking good, 5 has more detail but lines are getting large
    scale_colour_viridis(trans = "log10", begin = 0.125, end = 1, option = "viridis", direction = 1 ) +
    scale_alpha_continuous(range = c(0.5, 0.6), trans = "log") +
    theme_dark_map(base_size = 20) +
    guides(colour = FALSE,
           alpha = FALSE,
           size = FALSE) +
    # coord_quickmap(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
    coord_quickmap() +
    ggtitle("Chicago Divvy Most Popular Roads", subtitle = "2013 - 2017") +
    labs(caption = "delvinso.github.io") + 
    theme(plot.caption = element_text(size = 12))

  dev.off()
  end_time <- Sys.time()
  end_time - start_time

# Hi-Res ----

  start_time <- Sys.time()
  # png(filename = "divvy_routes_heatmap_log_01_hires_line_size_8.png", width = 1440, height = 2500, bg = "black")
  png(filename = "output/divvy_hires_log10_sz_05_5_qm_newest_legdb.png", width = 1440, height = 2500, bg = "black")

    # ggmap(chi3, darken = 0.85) +
  ggplot() +
    geom_leg(data = res_counts, aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat,
                                    colour = count, alpha = count, size = count, group = grp)) +
      # log1p(count/ max(count)
    geom_point(data = stations,
               aes(x = longitude, y = latitude),
               color = "#F05123", alpha = 0.7, size = 2.5) +
    scale_size_continuous(range = c(0.5, 5)) + #0.15 # 3 is looking good, 5 has more detail but lines are getting large
    scale_colour_viridis(trans = "log10", begin = 0.125, end = 1, option = "viridis", direction = 1 ) +
    scale_alpha_continuous(range = c(0.5, 0.9), trans = "log") +
    theme_dark_map(base_size = 50) +
    guides(colour = FALSE,
           alpha = FALSE,
           size = FALSE) +
    coord_quickmap(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
    coord_quickmap() +
    ggtitle("Chicago Divvy Most Popular Roads", subtitle = "2013 - 2017") +
    labs(caption = "delvinso.github.io") + 
    theme(plot.caption = element_text(size = 30))
  dev.off()
  end_time <- Sys.time()
  end_time - start_time
