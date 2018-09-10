library(tidyverse)
library(ggmap)
library(RPostgreSQL)

# Connection to PostgreSQL ----

divvy_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "chicago-divvy-data", host = "localhost")

# function to send SQL queries and return results as a tibble
query <- function(sql, con = divvy_con) {
  fetch(dbSendQuery(con, sql), n = 1e8) %>%
    as_tibble()}



# Getting Routes for All Active Station to Station Trips ----

stations <- query("SELECT * from stations;")

# This allows us to retrieve directions for every possible station to station journey for the static heatmap
latlon <- stations %>%
  mutate(coord = paste(latitude, longitude, sep = ", ")) %>%
  select(id, external_id, name, coord)

# for use with querying google maps api
sts_uniq <- query("SELECT
                   start_station_id, end_station_id, count(*) AS count,
                  COUNT(DISTINCT(date(start_time))) as num_days
                  FROM trips
                  WHERE start_station_id != end_station_id
                  GROUP BY start_station_id, end_station_id;")


# by default, route() returns results from overview_polyline, which is a summary the route and not a perfectly smooth path between locations, works for routes that are mostly straight lines
# what we need to do instead is, take each individual polyline and decode it, returning the latitutde and longitude
# to do that we need to return the whole structure of the query results using output = 'all'
# example

#(https://stackoverflow.com/questions/46479135/polyline-between-to-locations-not-snapped-to-road?noredirect=1&lq=1) and
# (https://stackoverflow.com/questions/46479135/polyline-between-to-locations-not-snapped-to-road?noredirect=1&lq=1).


# registering google API key for use with ggmap()
key <- "your_key_here"
register_google(key, day_limit = 500000)
ggmap_credentials()


# retrieving and decoding polylines of individual legs
dir <- ggmap::route(from = latlon$coord[latlon$id == 411],
                    to = latlon$coord[latlon$id == 420],
                    mode = "bicycling",
                    # alternatives = FALSE,
                    messaging = FALSE,
                    output = "all",
                    structure = "route",
                    override_limit = TRUE)
dir$routes[[1]]$legs[[1]]$steps[[1]]$polyline$points %>% decodeLine()

test <- map_dfr(dir$routes[[1]]$legs[[1]]$steps, function(leg){

  legPoly <- leg$polyline$points
  decode_legPoly <- decodeLine(legPoly)

  res <- data.frame(lat = decode_legPoly$lat,
                    lon = decode_legPoly$lon,
                    m = leg$distance$value,
                    minutes = leg$duration$value/60)

})

# retrieving and decodying polyline of overview
test2 <- dir <- ggmap::route(from = latlon$coord[latlon$id == 411],
                             to = latlon$coord[latlon$id == 420],
                             mode = "bicycling",
                             # alternatives = FALSE,
                             messaging = FALSE,
                             output = "simple",
                             structure = "route",
                             override_limit = TRUE)
# visualizing the difference in routes
ggplot() +
  geom_path(data = test2, aes(y = lat, x = lon), colour = "white") +
  geom_path(data = test, aes(y = lat, x = lon), colour = "lightgreen") +
  geom_point(data = stations ,
             aes(x = longitude, y = latitude),
             color = "#FDE725FF", alpha = 0.6, size = 0.1) +
  scale_size_continuous(range = c(0.134, 1), trans = "sqrt") +
  scale_colour_gradient(low = "#555555", high = "#ffffff", trans = "sqrt") +
  scale_alpha_continuous(range = c(0.14, 0.75), trans = "log") +
  theme_dark_map()

## There are over 120k unique station to station possibilities, to retrieve the routes for each one in serial takes approximately 17 hours.
## We can speed this up using foreach and DoParallel to parallelize the queries.
ggmap_credentials()


library(foreach)
library(doParallel)
cores = detectCores()
cl <- makeCluster(cores[1] -1) #not to overload your computer
registerDoParallel(cl)


start_time <- Sys.time()
# for(i in 1:dim(sts_uniq[1:1000, ])[1]){
res <- foreach (i = 1:dim(sts_uniq[1:10,])[1], .combine = rbind, .errorhandling = "remove", .packages = c("tidyverse", "ggmap")) %dopar% {
  options(stringsAsFactors = FALSE)

  key <- "your_key_here"
  register_google(key, day_limit = 500000)

  one <- sts_uniq[i, ]$start_station_id
  two <- sts_uniq[i, ]$end_station_id

  message(cat("Currently Retrieving Directions For: ", one, two, "Iteration", i))

  tryCatch(dir <- ggmap::route(from = latlon$coord[latlon$id == one],
                               to = latlon$coord[latlon$id == two],
                               mode = "bicycling",
                               # alternatives = FALSE,
                               # messaging = FALSE,
                               output = "all",
                               structure = "route",
                               override_limit = TRUE),
           error = function(e){NA},
           warning = function(w){NA})

  temp <- purrr::map_dfr(dir$routes[[1]]$legs[[1]]$steps, function(leg){

    legPoly <- leg$polyline$points
    decode_legPoly <- decodeLine(legPoly)

    legData <- data.frame(lat = decode_legPoly$lat,
                          lon = decode_legPoly$lon,
                          m = leg$distance$value,
                          minutes = leg$duration$value/60,
                          id_start = one,
                          id_end = two)
    return(legData)
  })

}
end_time <- Sys.time()
end_time - start_time
stopCluster(cl)
# writing to csv
write_csv(res, "data/legs_raw.csv")
# use lead to create end points for lat and lon for use with geom_segment()
res_total <- res %>%
  group_by(id_start, id_end) %>%
  mutate(end_lat = lead(lat), end_lon = lead(lon)) %>%
  rename(start_lat = lat, start_lon = lon) %>%
  select(id_start, id_end, start_lat, end_lat, start_lon, end_lon, m, minutes)
# write to csv
write_csv(res_total, "data/legs_start_end.csv")
# this will be read into the postgresql DB


# join the leg data with the station to station counts, will be used in plotting the static heat map
# res_counts <- res_total %>%
#   inner_join(sts_uniq, by = c("id_start" = "start_station_id", "id_end" = "end_station_id" )) %>%
#   # create groups so ggmap knows how the legs should be connected
#   mutate(grp = (paste(id_start, id_end, sep = "."))) %>%
#   # use lead to create end points for lat and longitude
#   group_by(grp) %>%
#   arrange(count) %>%
#   mutate(end_lat = lead(lat), end_lon = lead(lon)) %>%
#   rename(start_lat = lat, start_lon = lon)


# in serial ----

start_time <- Sys.time()

test <- for(i in 1:dim(sts_uniq[1:10, ])[1]){

  one <- sts_uniq[i, ]$start_station_id
  two <- sts_uniq[i, ]$end_station_id

  message(cat("Currently Retrieving Directions For: ", one, two, "Iteration", i))

  tryCatch(dir <- ggmap::route(from = latlon$coord[latlon$id == one],
                               to = latlon$coord[latlon$id == two],
                               mode = "bicycling",
                               # alternatives = FALSE,
                               # messaging = FALSE,
                               output = "all",
                               structure = "route",
                               override_limit = TRUE),
           error = function(e){NA},
           warning = function(w){NA})

  # if(is.na(dir)) dir <- data.frame(m = NA, km = NA, miles = NA, seconds = NA, minutes = NA, hours = NA, startLon = NA, startLat = NA, endLon = NA, endLat = NA, leg = NA)

  temp <- purrr::map_dfr(dir$routes[[1]]$legs[[1]]$steps, function(leg){

    legPoly <- leg$polyline$points
    decode_legPoly <- decodeLine(legPoly)

    legData <- data.frame(lat = decode_legPoly$lat,
                          lon = decode_legPoly$lon,
                          m = leg$distance$value,
                          minutes = leg$duration$value/60,
                          id_start = one,
                          id_end = two)
    return(legData)
  })
  # #
  # #   if(any(is.na(dir)) | any(is.na(temp))) temp <- data.frame(lat = NA, lon = NA, m = NA, minutes = NA, id_start = one, id_end = two)
  #
  # temp <-  decode %>% mutate(id_start = one, id_end = two)
  #
  # temp <-  dir %>% mutate(id_start = one, id_end = two)

  # for use when querying serially
  if(exists("res")){
    # if res does exist, then add to it
    res <- bind_rows(res, temp)
  } else{
    # else create res
    res <- temp
  }


}
end_time <- Sys.time()
end_time - start_time
write_csv(res, "legs_raw.csv")


# other stuff ----
  # iterate through each 'routes' object
  # res2 <- res
  res_decoded <- res2 %>% mutate(

    coords = purrr::map(res[, 2], function(results){

      # for each routes object, iterate through all possible legs and return thedecoded polyline for a given point
      purrr::map(results[[1]]$legs[[1]]$steps, function(leg){

        legPoly <- leg$polyline$points
        decode_legPoly <- decodeLine(legPoly)

        legData <- data.frame(lat = decode_legPoly$lat,
                              lon = decode_legPoly$lon,
                              m = leg$distance$value,
                              minutes = leg$duration$value/60)
      })
    }))

  # google map styling - not needed
  # removing all labels
  style1 = c(feature = "all", element = "labels", visibility = "simplified")

  style2 <- c("&style=", feature = "road", element = "labels", visibility = "off")
  # style2 <- c(feature = "road.highway")
  # filling all buildings as black
  style3 <- c("&style=", feature = "landscape", element = "geometry.fill", color = "0x000000")
  # filling all poi as black
  style4 <- c("&style=", feature = "poi", element = "geometry.fill", color = "0x000000")
  # filling all administrative features as black
  style5 <- c("&style=", feature = "administrative", element = "geometry.fill", color = "0x000000")
  # filling all bodies of water as black
  style6 <- c("&style=", feature = "water", element = "geometry.fill", color = "0x000000")
  # filling all transit as black
  style7 <- c("&style=", feature = "transit", element = "geometry.fill", color = "0x000000")
  # filling all roads
  style8 <- c("&style=", feature = "road", element = "geometry", color = "0x000000", visibility = "simplified")


  # tunring visibility of all features off and filling in what i need
  style1 <- c(feature = "all", element = "geometry", color = "0x000000")
  style2 <- c("&style=",feature = "all", element = "labels", visibility = "off" )
  # displaying neighbourhood names
  style3 = c("&style=", feature = "administrative.neighborhood", element = "labels.text.fill", visibility = "on", color = "0xd3d3d3", weight = 0.1)
  style4 = c("&style=", feature = "administrative", visibility = "on", color = "0xffffff")


  # https://stackoverflow.com/questions/41049782/making-multiple-style-references-in-google-maps-api?noredirect=1&lq=1
  # style <- c(style1, style2, style3, style4, style5, style6, style7, style8)
  style <- c(style1, style2, style3)
  chi_map <- get_googlemap("chicago", source = "terrain",
                           zoom = 11, color = "bw",
                           # maptype = "toner",
                           style = style)
  # chi_map <- get_map(location = c(-87.827344, 41.708798, -87.532917, 42.092102), source = "osm", color = "bw")
  ggmap(chi_map)
  # geom_polygon(data = tracts.map, aes(x = long, y = lat, group = group), alpha = 0.5)# +
  coord_map(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat))

  chi_map <- get_googlemap("chicago", source = "terrain",
                           zoom = 11, color = "bw")


  min_lat <- 41.716999
  max_lat <- 42.081900
  min_lon <- -87.827344
  max_lon <-  -87.532917
  g1 + annotation_custom(grob = ggplotGrob(
    ggmap(chi_map) +
      coord_map(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat))
  ), xmin = 41.716999, xmax = 42.081900,
  ymin = -87.827344, ymax = -87.532917)
  # end other stuff ----
