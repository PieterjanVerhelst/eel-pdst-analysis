# eel-pdst-analysis
# Analysis of pDST data from European eels
Pop-off data storage tags (pDSTs) log data (e.g. water temperature and pressure) during a fish's trajectory. After a preprogrammed time, the tag detaches, surfaces, drifts ashore and can be found by, for instance, beachcombers and fishermen who send the tag back to us. Consequently, each datafile contains information related to the trajectory of 1 fish, logging multiple variables (e.g. water temperature and pressure) which can be at different rates. This data allows us to reconstruct a fish's trajectory in the marine environment.

## Project structure



### Data

* `/raw:`
	This folder contains the raw download files with temperature and depth data from the archival tags (pDST and PSAT)
	the folder `non-processed-files`contains raw files that have not been processed and put in the `interim` folder

* `/interim:`
	+ This folder contains the processed raw data, which has been split into a daylog- and sensor-file. Daylog-files contain daily summaries of temperature and pressure data (min, mean and max). The sensor-files contain the raw temperature and pressure data.
	+ `/input_tagID:` The 'tag_ID' corresponds to the actual tag ID of a tag. Each folder contains 4 input files for the geolocation model:
		+ `tagID_LONG.csv`: linear interpolation of longitude between start and end date
		+ `tagID_PRES.csv`: depth data (corrected for depth drift)
		+ `tagID_TEMP.csv`: temperature data
		+ `tagID_TEMP_F.csv`: average temperature at sea surface (first 20 m) and daily max depth

* `/external:`
	+ `/geolocation_input_files:` input files for the geolocation model per eel with lat and lon of release and retrieval or pop-off location
	+ `/trajectory_data:` output from geolocation model with calculated daily lat and lon, and daily summary on temperature and depth (files obtained by David Righton)


### Scripts

! Important note: the clock settings of the pDSTs are the same as Pieterjan's laptop, hence their time setting is UTC+1 (Brussels time zone)!

* `/src:`

1. pdst_functions.R: Functions to serialize/read and tidy pDST datasets
2. pdst_raw_to_tidy.R: Convert pDST csv files into tidy pDST csv files and store them in `\data\interim\`
3. create_temperature_depth_plots.R: Create plots with the logged temperature and depth over time
4. interpolate_longitude.R: Create input file for model: longitude interpolation
5. create_t_and_p_files.R: Create input file for model: temperature and pressure files
6. create_sst_file.R: Create file with daily sea surface temperatures (first 20 m depth) and max depths
7. read_trajectory_data.R: Batch read and process trajectory data obtained via matlab toolbox which is stored in ./data/external/trajectory_data
8. analyse_migration_speed.R: Calculate and analyse horizontal migration speeds


