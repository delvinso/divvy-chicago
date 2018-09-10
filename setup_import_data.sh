
#!/bin/bash

###### Download Raw Files ######


##### Database ######

createdb chicago-divvy-data

###### Create Schema ######
echo "Creating schema and importing files..."
psql chicago-divvy-data -f setup_scripts/01_create_schema.sql


###### Importing Trip, Station & Geographical Data ######
echo "Importing raw trips"

# raw trips

# get absolute paths to use with COPY
# fileOne = $(find $PWD/data | egrep "raw_combined.csv")
# fileTwo = $(find $PWD/data | egrep 'Divvy_Stations_2017_Q3Q4.csv')
# fileThree = $(find $PWD/data | egrep 'gsod_ohare.csv')

# find $PWD/data helps circumvent absolute pathway requirement for postgresql's copy

psql chicago-divvy-data -c "SET datestyle = 'ISO, MDY'; COPY trips_raw FROM '$(find $PWD/data | egrep "raw_combined.csv")' CSV HEADER;"


echo "Importing station data"
# raw stations
psql chicago-divvy-data -c "SET datestyle = 'ISO, MDY'; COPY stations_raw FROM '$(find $PWD/data | egrep "Divvy_Stations_2017_Q3Q4.csv")' CSV HEADER;"

echo "Importing weather data"

# weather data
psql chicago-divvy-data -c "SET datestyle = 'ISO, MDY'; COPY chicago_weather_observations FROM '$(find $PWD/data | egrep "gsod_ohare.csv")' CSV HEADER;"


# gis stuff

  ## Load Census Tract Spatial Data + Create Indexes

  ## check .prj file for SRID
echo "Importing census shapefiles"

shp2pgsql -s 4326 data/chicago_ct_2010/chicago_ct_2010.shp   | psql -d chicago-divvy-data
psql chicago-divvy-data -c "CREATE INDEX index_chict_geom ON chicago_ct_2010 USING gist (geom);"
psql chicago-divvy-data -c "VACUUM ANALYZE chicago_ct_2010"

echo "Importing community area shapefiles"

  ## Load Community Area Geospatial Data + create Indexes
shp2pgsql -s 4326 data/community_area/chicomm2018.shp | psql -d chicago-divvy-data
psql chicago-divvy-data -c "CREATE INDEX index_bound_geom ON chicomm2018 USING gist (geom);"
psql chicago-divvy-data -c "VACUUM ANALYZE chicomm2018"

###### tidies trip and station data as well as maps stations to census tracts and community areas ######

echo "Cleaning stuff and mapping stations to ct and comm areas"

psql chicago-divvy-data -f setup_scripts/02_import.sql


######  create tables for analyses ######

echo "Creating tables for analyses"

psql chicago-divvy-data -f setup_scripts/03_tables_for_analyses.sql
