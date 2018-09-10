

-- Inserting relevant station data from raw
 INSERT INTO stations (external_id, name, latitude, longitude)
 SELECT DISTINCT
   station_id, station_name,
   ROUND(latitude, 6), ROUND(longitude, 6)
 FROM stations_raw
 ON CONFLICT DO NOTHING;

 -- creating station geometry column, used for for mapping stations to shapefiles
  SELECT AddGeometryColumn('stations', 'geom', 4326, 'POINT', 2);
  CREATE INDEX index_stations_on_geom ON stations USING gist (geom);

-- geom from station lat/long

 UPDATE stations
 SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)
 WHERE geom IS NULL;

 CREATE UNIQUE INDEX idx_stations_unique ON stations (external_id, latitude, longitude);

-- Inserting combined trip and station data into trips
INSERT INTO trips (
  trip_duration,
  start_time,
  stop_time,
  start_station_id,
  end_station_id,
  bike_id,
  user_type,
  birth_year,
  gender)
SELECT
  trip_duration,
  start_time,
  stop_time,
  ss.id,
  es.id,
  bike_id,
  user_type,
  NULLIF(NULLIF(birth_year, ''), 'NULL')::int,
  gender
  FROM trips_raw t
    INNER JOIN stations ss
      ON t.start_station_id = ss.external_id
    INNER JOIN stations es
      ON t.end_station_id = es.external_id;

-- drop contents of trips_raw but retains the schema
 TRUNCATE TABLE trips_raw;

 -- Mapping Stations to Census Tracts + Community Areas



 UPDATE stations
 SET chict2010_gid = c.gid
 FROM chicago_ct_2010 c
 WHERE stations.chict2010_gid IS NULL
   AND ST_Within(stations.geom, ST_Transform(c.geom, 4326));


 UPDATE stations
 SET commarea = a.community,
     commid = a.gid
 FROM chicomm2018 a
 WHERE stations.commid IS NULL
   AND ST_Within(stations.geom, ST_Transform(a.geom, 4326));


   -- Create indexes

  CREATE UNIQUE INDEX index_weather_observations ON chicago_weather_observations (date);
   CREATE INDEX idx_trips_start_station ON trips (start_station_id);
   CREATE INDEX idx_trips_end_station ON trips (end_station_id);
   CREATE INDEX idx_trips_dow ON trips (EXTRACT(DOW FROM start_time));
   CREATE INDEX idx_trips_hour ON trips (EXTRACT(HOUR FROM start_time));
   CREATE INDEX idx_trips_date ON trips (date(start_time));
