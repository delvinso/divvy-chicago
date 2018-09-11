
### Divvy Bikeshare PostgreSQL Database & Analyses

This repo contains the code necessary to setup the database and all analyses used in my post [14 Million Bike Rides in The Windy City (2013 - 2017) - An Analysis of Chicago’s Bikeshare System](https://delvinso.github.io/project/14-million-bike-rides-chicago-divvy/).

##### 1. Download the bikeshare data.

`download_concat_data.sh`

Downloads the raw trip data from 2013 - 2017 into the sub-directory /data within the current folder and concatenates the raw trip data into one file.

##### 2. Run `setup_import_data.sh`, which does the following:

###### a) Creates a database named 'divvy-bikeshare-data' with appropriate schema.

`01_create_schema.sql`

###### b) Imports raw trips, station data, community areas and census tracts.

`02_import.sql`

###### c) Cleans the raw trip and station data and maps the station data onto community areas and census tracts

`03_tables_for_analyses.sql`



### Analyses

SQL queries and analyses can be found in `analyses/`.


4) 04_get_legs_routes.R DON"T FORGET TO REMOVE THE KEY DELVIN!!!!!!!!



#### Static Heat Map of Trips

Additionally, if one wants to recreate the static heat map in ....., look at 04_get_legs_routes.R.

##### NOTE: This may cost $$$$ with the new pricing changes Google implemented with their Google Maps API mid-July 2018. May want to consider looking into OSRM as a free, open-source alternative.


The legs can then be imported using `04_import_legs.sh`, making sure to run it from the top level directory of the repo or

`psql chicago-divvy-data -c "SET datestyle = 'ISO, MDY'; COPY legs FROM 'ABSOLUTE_PATH/data/legs_start_end.csv' CSV HEADER;". `

#### TO DO

* Review Indexes
* Review transports.R - OVER and WINDOW functions
