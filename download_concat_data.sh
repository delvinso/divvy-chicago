
#!/bin/bash


###### Download Raw Files ######

echo "Downloading raw trip data into /data"
cat setup_scripts/trip_urls.txt | xargs -n 1 -P 6 wget -P data/
unzip 'data/*.zip' -d data/
# rm data/Divvy_*.zip


######  Combining Raw Trip Data ######
echo "Concatenating raw trip data"
if [ ! -e data/raw_combined.csv ]; then
  echo "Creating raw_combined.csv"
  for f in data/Divvy_Trips*.csv data/**/Divvy_Trips*.csv ; do
      tail -n +2 "$f" >> data/raw_combined.csv # start at line 2 to remove the header
      echo "processing $f"
    done
  else
    echo "raw_combined.csv already exists, skipping.."
fi
