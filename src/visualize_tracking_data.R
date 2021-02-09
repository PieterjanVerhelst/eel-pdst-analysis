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
data <- read.csv("./data/interim/data_circadian_tidal.csv")
data$X <- NULL
# data$ID <- factor(data$ID)
data$datetime  <- as_datetime(data$datetime)

# Remove NAs
data <- data[!is.na(data$lat), ]
data <- data[!is.na(data$lon), ]

# ID eels in data
unique(data$ID)
#' Filter eels for tracking visualization
#' select all by:
#' eels_ID <- unique(data$ID)

eels_ID <- c(15714, 15777)
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
eels_cols <- c("red", "blue", "green", "black", "yellow")

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
                           "15714" = "Flotsam",
                           "15777" = "Jetsam"))

# use df2move to convert the data.frame into a moveStack
move_data <- 
  df2move(data,
          proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "datetime", track_id = "nickname")

# align move_data to a uniform time scale
m <- align_move(move_data, res = 1, unit = "days")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = eels_cols,
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()


frames[[length(frames)]] # preview last frame

# animate frames
animate_frames(frames,
               out_file = glue("visualization_trajectory_ID_{ id }.gif",
                               id = glue_collapse(unique(data$nickname), sep = "-")),
               overwrite = TRUE)

