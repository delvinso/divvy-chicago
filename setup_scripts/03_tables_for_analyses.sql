

---- anonymous
-- hourly
CREATE TABLE anon_hourly AS
SELECT
    date_trunc('hour', start_time + interval '30 minute') round_hour,
    start_station_id,
    gender,
    EXTRACT(YEAR FROM start_time) - birth_year::int AS age,
    COUNT(*) AS count
  FROM trips
  WHERE
    user_type = 'Subscriber'
    AND gender IS NOT NULL
    AND birth_year::int IS NOT NULL
    AND birth_year::int >= 1940
    AND birth_year::int <= 2005
  GROUP BY round_hour, start_station_id, gender, age;
-- daily
CREATE TABLE anon_daily AS
SELECT
    date_trunc('day', start_time + interval '12 hour') round_day,
    start_station_id,
    gender,
    EXTRACT(YEAR FROM start_time) - birth_year::int AS age,
    COUNT(*) AS count
  FROM trips
  WHERE
    user_type = 'Subscriber'
    AND gender IS NOT NULL
    AND birth_year::int IS NOT NULL
    AND birth_year::int >= 1940
    AND birth_year::int <= 2005
  GROUP BY round_day, start_station_id, gender, age;


  CREATE table daily_agg_weather AS
  WITH aggregate_data AS(
    SELECT
      date(start_time) AS date,
      COUNT(*) as trips
    FROM trips
    GROUP BY date(start_time)
    )
  SELECT a.*,
    w.average_temp,
    w.visibility,
    w.average_wind_speed,
    w.max_temp,
    w.min_temp,
    w.precip,
    w.dewp,
    w.snow_depth_mm,
    w.fog,
    w.rain_drizzle,
    w.snow_ice,
    w.hail,
    w.thunder,
    w.tornado_funnel,
    w.relative_humidity,
    EXTRACT(DOW FROM a.date) AS dow,
    EXTRACT(MONTH FROM a.date) AS month,
    EXTRACT(YEAR from a.date) AS year,
    a.date IN ('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25') AS holiday
FROM aggregate_data a
 INNER JOIN chicago_weather_observations w
   ON a.date = w.date
ORDER BY a.date;

--- weekday station to station counts
CREATE TABLE weekday_sts_hourly AS
  SELECT
    date(start_time) AS date,
    EXTRACT(HOUR from start_time) AS hour,
    start_station_id,
    end_station_id,
    COUNT(*) AS trips,
    date(start_time) IN  ('2013-07-04', '2013-09-02', '2013-11-28', '2013-11-29', '2013-12-24', '2013-12-25', '2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27', '2014-11-28', '2014-12-24', '2014-12-25', '2015-01-01', '2015-01-19', '2015-02-16', '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26', '2015-11-27', '2015-12-24', '2015-12-25') AS holiday
  FROM trips
  GROUP BY date, hour, start_station_id, end_station_id;
CREATE INDEX idx_station_to_station ON weekday_sts_hourly (start_station_id, end_station_id);


-- bike availability rebalancing (did not edit this code, copied verbatim from Todd's work, need to understand window..)
CREATE TABLE daily_unique_bike_ids AS
SELECT DISTINCT date(start_time) AS date, bike_id
FROM trips
ORDER BY date;
CREATE INDEX idx_daily_unique_bikes ON daily_unique_bike_ids (date);

CREATE TABLE monthly_active_bikes AS
SELECT
  date(d) AS date,
  COUNT(DISTINCT b.bike_id) AS bikes
FROM generate_series('2013-01-01'::date, '2017-12-31'::date, '1 day'::interval) d,
     daily_unique_bike_ids b
WHERE b.date <= date(d)
  AND b.date >= date(d) - '27 days'::interval
GROUP BY date(d)
ORDER BY date(d);

CREATE TABLE bike_station_ids AS
SELECT
  id,
  bike_id,
  start_time,
  stop_time,
  end_station_id,
  lead(start_station_id, 1) OVER w AS next_start_station_id,
  lead(start_time, 1) OVER w AS next_start_time
FROM trips
WINDOW w AS (PARTITION BY bike_id ORDER BY start_time)
ORDER BY bike_id, start_time;
DELETE FROM bike_station_ids WHERE next_start_station_id IS NULL;

CREATE TABLE station_aggregates AS
SELECT
  end_station_id,
  COUNT(*)::numeric AS total_drop_offs,
  SUM((end_station_id != next_start_station_id)::int) AS transported_to_other_station
FROM bike_station_ids
GROUP BY end_station_id;

CREATE TABLE monthly_station_aggregates AS
SELECT
  date(date_trunc('month', stop_time)) AS month,
  end_station_id,
  COUNT(*)::numeric AS total_drop_offs,
  SUM((end_station_id != next_start_station_id)::int) AS transported_to_other_station
FROM bike_station_ids
GROUP BY end_station_id, month;

CREATE TABLE hourly_station_aggregates AS
SELECT
  EXTRACT(HOUR FROM stop_time) AS hour,
  end_station_id,
  COUNT(*)::numeric AS total_drop_offs,
  SUM((end_station_id != next_start_station_id)::int) AS transported_to_other_station
FROM bike_station_ids
GROUP BY end_station_id, hour;
--
-- SELECT SUM(transported_to_other_station) / SUM(total_drop_offs)
-- FROM station_aggregates;
--
-- SELECT
--   ntaname,
--   SUM(transported_to_other_station) / SUM(total_drop_offs) frac,
--   SUM(total_drop_offs) total
-- FROM station_aggregates a
--   INNER JOIN stations s
--   ON a.end_station_id = s.id
-- GROUP BY ntaname
-- ORDER BY frac DESC;
--
-- SELECT
--   ntaname,
--   hour,
--   SUM(transported_to_other_station) / SUM(total_drop_offs) frac,
--   SUM(total_drop_offs) total
-- FROM hourly_station_aggregates a
--   INNER JOIN stations s
--   ON a.end_station_id = s.id
-- GROUP BY ntaname, hour
-- HAVING SUM(total_drop_offs) > 1000
-- ORDER BY ntaname, hour;

