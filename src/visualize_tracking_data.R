#' Visualize tracking data as animated gif using moveVis package
#' https://movevis.org/index.html
#' 
#' LifeWatch 2021
#' Damiano Oldoni
#' damiano.oldoni@inbo.be

# Load packages
library(moveVis)
library(move)
library(lubridate)
library(tidyverse)
library(glue)

# Load dataset with all eels
data <- read.csv("./data/interim/batch_processed_eels_5min_totaltrack.csv")
data$X <- NULL
data$ID <- factor(data$ID)
data$datetime  <- as_datetime(data$datetime)

# Remove NAs
data <- data[!is.na(data$geoloc_avg_lat), ]
data <- data[!is.na(data$geoloc_avg_lon), ]

# ID eels in data
unique(data$ID)
#' Filter eels for tracking visualization
#' select all by:
#' eels_ID <- unique(data$ID)

# Select 2019 eels
eels_ID <- c(17443, 17499, 17513, 17534, 17526, 17522, 17508, 17536, 17538, 17537, 17510, 15789) 
# eels_ID <- unique(data$ID)
data <- data %>% filter(ID %in% eels_ID)

# Check min max datetime for each eel to see if they are compatible (same time slot)
data %>%
  group_by(ID) %>%
  summarise(start = min(datetime),
            end = max(datetime))

# undersampling to one point per day
data <- 
  data %>%
  group_by(ID) %>%
  filter(hour(datetime) == 0 & 
           minute(datetime) == 0 &
           # this should not be needed as data are sampled by minute
           second(datetime) == 0) %>%
  ungroup()

#' set color vector
#' this example allows max 5 individuals. Add more colors otherwise
eels_cols <- c("red", "blue", "darkgreen", "black", "yellow", "pink", "purple", "brown", "orange", "darkgray", "cyan", "chartreuse")

# trim color vector based on number of tracked individuals in data
eels_cols <- eels_cols[1:length(eels_ID)]

# enable multi-core usage automatically
use_multicore()

# allow disk use with default directory
# and maxiumum of 50 frames in memory
use_disk(frames_to_disk = TRUE, n_memory_frames = 50)


#' Add nicknames
#' Duel moray eels from The Little Mermaid
#' https://disney.fandom.com/wiki/Flotsam_and_Jetsam
data <-
  data %>%
  mutate(nickname = recode(as.character(ID),
                           "17443" = "17443",
                           "17499" = "17499",
                           "17513" = "17513",
                           "17534" = "17534",
                           "17526" = "17526",
                           "17522" = "17522",
                           "17508" = "17508",
                           "17536" = "17536",
                           "17538" = "17538",
                           "17537" = "17537",
                           "17510" = "17510",
                           "15789" = "15789"))

# use df2move to convert the data.frame into a moveStack
move_data <- 
  df2move(data,
          proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
          x = "geoloc_avg_lon", y = "geoloc_avg_lat", time = "datetime", track_id = "nickname")

# align move_data to a uniform time scale
m <- align_move(move_data, res = 1, unit = "days")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = eels_cols,
                         map_service = "osm", map_type = "terrain_bg", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()


frames[[length(frames)]] # preview last frame

# animate frames
animate_frames(frames,
               fps = 10,
               out_file = glue("./data/output/visualization_trajectory_ID_{ id }.gif",
                               id = glue_collapse(unique(data$nickname), sep = "-")),
               overwrite = TRUE)

