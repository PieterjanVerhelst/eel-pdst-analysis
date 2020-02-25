# eel-pdst-analysis
# Analysis of pDST data from European eels
Pop-off data storage tags (pDSTs) log data (e.g. water temperature and pressure) during an fish's trajectory. After a preprogrammed time, the tag detaches, surfaces, drifts ashore and can be found by, for instance, beachcombers and fishermen who send the tag back to us. Consequently, each datafile contains information related to the trajectory of 1 fish, logging multiple variables (i.e. water temperature and pressure) which can be at different rates. This data allows us to reconstruct a fish's trajectory in the marine environment.

## Project structure

### Data-analyse

* `/src:`

1. pdst_functions.R: Functions to serialize/read and tidy pDST datasets
2. pdst_raw_to_tidy.R: Convert pDST csv files into tidy pDST csv files and store them in `\data\interim\`
3. create_temperature_depth_plots.R: Create plots with the logged temperature and depth over time
4. interpolate_longitude.R: Create input file for model: longitude interpolation
5. create_t_and_p_files.R: Create input file for model: temperature and pressure files
6. create_sst_file.R: Create file with daily sea surface temperatures (first 20 m depth) and max depths