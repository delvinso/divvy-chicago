
# run get_legs_routes.R to generate all possible legs between station to station before importing into database
psql chicago-divvy-data -c "SET datestyle = 'ISO, MDY'; COPY legs FROM '$(find $PWD/data | egrep "legs_start_end.csv")' CSV HEADER NULL AS 'NA';"
