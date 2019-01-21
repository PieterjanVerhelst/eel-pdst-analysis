# eel-pdst-analysis
# Analysis of pDST data from European eels
Pop-off data storage tags (pDSTs) log data (e.g. water temperature and pressure) during an fish's trajectory. After a preprogrammed time, the tag detaches, surfaces, drifts ashore and can be found by, for instance, beachcombers and fishermen who send the tag back to us. Consequently, each datafile contains information related to the trajectory of 1 fish, logging multiple variables (i.e. water temperature and pressure) which can be at different rates. This data allows us to reconstruct a fish's trajectory in the marine environment.

## Project structure

### Data-analyse

* `/src:`

1. Processing.R: upload all datafiles, remove redundant rows and combine them into 1 dataset.