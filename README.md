# eel-pdst-analysis
# Analysis of pDST data from European eels
Pop-off data storage tags (pDSTs) log data (e.g. water temperature and pressure) during a fish's trajectory. After a preprogrammed time, the tag detaches, surfaces, drifts ashore and can be found by, for instance, beachcombers and fishermen who send the tag back to us. Consequently, each datafile contains information related to the trajectory of 1 fish, logging multiple variables (e.g. water temperature and pressure) which can be at different rates. This data allows us to reconstruct a fish's trajectory in the marine environment.

## Project structure



### Data

* `/raw:`
	This folder contains the raw download files with temperature and depth data from the archival tags (pDST and PSAT).
	The folder `non-processed-files`contains raw files that have not been processed and put in the `interim` folder.

* `/interim:`
	+ `/daylogs:` The daylog-files contain daily summaries of temperature and pressure data (min, mean and max). 
	+ `/sensorlogs:` The sensor-files contain the raw temperature and pressure data.
	+ `/geolocation_input_files:` 
		+ `/input_tagID:` The 'tag_ID' corresponds to the actual tag ID of a tag. Each folder contains 4 input files for the geolocation model:
			+ `tagID_LONG.csv`: linear interpolation of longitude between start and end date
			+ `tagID_PRES.csv`: depth data (corrected for depth drift)
			+ `tagID_TEMP.csv`: temperature data
			+ `tagID_TEMP_F.csv`: average temperature at sea surface (first 20 m) and daily max depth
	+ `/dvm_noon:` Files with the calculated noon, based on the DVM behaviour

* `/external:`
	+ `/geolocation_input_files`: input files for the geolocation model per eel with lat and lon of release and retrieval or pop-off location
	+ `/trajectory_data`: output from geolocation model with calculated daily lat and lon, and daily summary on temperature and depth (files obtained by David Righton)
	+ `/dvm_noon`: Equation of time applied to files with the calculated noon, based on the DVM behaviour, to calculate coordinates
	+ `parameters.csv`: file containing the parameters to batch process the sensor files
		+ `start_datetime`: moment eel was released (GMT)
		+ `end_datetime`: end point of data of interest; in this case when eel is on the continental shelf (GMT)
			- till DVM
			- till one hour before predation
			- 15 minutes before popoff time
		+ `bank_datetime`: moment eel was on the river bank, i.e. start_datetime - 15 min (GMT)
		+ `popoff_datetime`: moment the tag reached the surface (GMT)
		+ `UTC`: UTC time correction (in hours) to convert to GMT 
		+ `pressure_correction`: indicates if a pressure correction needs to be applied (0 =  no, 1 = yes)
		+ `comment`: any comments

### Scripts

! Important note: the clock settings of the pDSTs are the same as Pieterjan's laptop, hence their time setting is UTC+1 (Brussels time zone)!

* `/src:`

1. pdst_functions.R: Functions to serialize/read and tidy pDST datasets
2. pdst_raw_to_tidy.R: Convert pDST csv files into tidy pDST csv files and store them in `/data/interim/`
3. create_temperature_depth_plots.R: Create plots with the logged temperature and depth over time
4. calculate_noon: Calculate noon based on DVM behaviour. This noon can be used to calculate lat and lon with the equation of time (in excel)
5. interpolate_longitude.R: Create input file for model: longitude interpolation
6. create_t_and_p_files.R: Create input file for model: temperature and pressure files
7. create_sst_file.R: Create file with daily sea surface temperatures (first 20 m depth) and max depths
8. read_trajectory_data.R: Batch read and process trajectory data obtained via matlab toolbox which is stored in `./data/external/trajectory_data`
9. analyse_migration_speed.R: Calculate and analyse horizontal migration speeds
10. batch_all_eels.R: Create sensor log file with all eels for analysis, taking into account time settings (UTC), pressure drift and redundant data (e.g. data when not on the shelf, during DVM and predation)
	+ Generate `batch_processed_eels.csv` in `interim` folder
11. link_circadian.R: Link circadian phases to the dataset
	+ Generate `data_circadian.csv` in `interim` folder
12. link_tidal.R: Link tidal data to the dataset (tidal data obtained via John Aldridge (CEFAS, UK))
	+ Generate `data_circadian_tidal.csv` in `interim` folder
13. calc_depth_diff.R: calculate depth difference between minima and maxima
	+ Generate `data_depth_diff.csv` in `interim` folder
14. create_plots.R: Create plots for data exploration


