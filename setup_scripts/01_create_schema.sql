



CREATE EXTENSION postgis;



CREATE TABLE trips_raw (
  trip_id integer ,
  start_time timestamp without time zone,
  stop_time timestamp without time zone,
  bike_id numeric,
  trip_duration numeric,
  start_station_id integer,
  start_station_name varchar,
  end_station_id integer,
  end_station_name varchar,
  user_type varchar,
  gender varchar,
  birth_year varchar
);


CREATE TABLE stations_raw (
  station_id integer,
  station_name varchar,
  city varchar,
  latitude numeric,
  longitude numeric,
  dpcapacity integer,
  online_date date,
  blank varchar
);


CREATE TABLE chicago_weather_observations (
  date date,
  average_temp numeric,
  visibility numeric,
  average_wind_speed numeric,
  max_temp numeric,
  min_temp numeric,
  precip numeric,
  dewp numeric,
  snow_depth_mm numeric,
  fog boolean,
  rain_drizzle boolean,
  snow_ice boolean,
  hail boolean,
 thunder boolean,
 tornado_funnel boolean,
 relative_humidity numeric);


 CREATE TABLE trips (
   id serial primary key,
   trip_duration numeric,
   start_time timestamp without time zone,
   stop_time timestamp without time zone,
   start_station_id integer,
   end_station_id integer,
   bike_id integer,
   user_type varchar,
   birth_year integer,
   gender varchar
 );



 CREATE TABLE stations (
   id serial primary key,
   external_id integer,
   name varchar,
   latitude numeric,
   longitude numeric,
   chict2010_gid integer,
   commid integer,
   commarea varchar

 );

 CREATE TABLE legs (
   start_station_id integer,
   end_station_id integer,
   start_lat numeric,
   end_lat numeric,
   start_lon numeric,
   end_lon numeric,
   m numeric,
   minutes numeric
 );


 -- combined trips and stations
   CREATE VIEW trips_and_stations AS (
     SELECT
       t.*,
       ss.name AS start_station_name,
       ss.latitude AS start_station_latitude,
       ss.longitude AS start_station_longitude,
       ss.chict2010_gid AS start_chict2010_gid,
       ss.commarea AS start_commarea,
       ss.commid AS start_commid,
       es.name AS end_station_name,
       es.latitude AS end_station_latitude,
       es.longitude AS end_station_longitude,
       es.chict2010_gid AS end_chict2010_gid,
       es.commarea AS end_commarea,
       es.commid AS end_commid
     FROM trips t
       INNER JOIN stations ss ON t.start_station_id = ss.id
       INNER JOIN stations es ON t.end_station_id = es.id
   );
