# Link data from NOAA (e;g. cloud cover and atmospheric pressure) to dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Load packages
library(tidyverse)
library(lubridate)
library(worldmet)
library(sf)
library(purrr)

# Check stations on map
info <- getMeta(lat = 55.5, lon = 7.5)

# 1. Download NOAA data ####
andrew <- importNOAA(code = "031405-99999", hourly = TRUE, year = 2018:2019)
andrew <- select(andrew, station, date, latitude, longitude, atmos_pres, cl_1, cl)

sleipner <- importNOAA(code = "010886-99999", hourly = TRUE, year = 2018:2019)
sleipner <- select(sleipner, station, date, latitude, longitude, atmos_pres, cl_1, cl)

lerwick <- importNOAA(code = "030050-99999", hourly = TRUE, year = 2018:2019)
lerwick <- select(lerwick, station, date, latitude, longitude, atmos_pres, cl_1, cl)

gullfax <- importNOAA(code = "013755-99999", hourly = TRUE, year = 2018:2019)
gullfax <- select(gullfax, station, date, latitude, longitude, atmos_pres, cl_1, cl)

bruce <- importNOAA(code = "031402-99999", hourly = TRUE, year = 2018:2019)
bruce <- select(bruce, station, date, latitude, longitude, atmos_pres, cl_1, cl)

heimdal <- importNOAA(code = "010875-99999", hourly = TRUE, year = 2018:2019)
heimdal <- select(heimdal, station, date, latitude, longitude, atmos_pres, cl_1, cl)

harding <- importNOAA(code = "031403-99999", hourly = TRUE, year = 2018:2019)
harding <- select(harding, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ula <- importNOAA(code = "014281-99999", hourly = TRUE, year = 2018:2019)
ula <- select(ula, station, date, latitude, longitude, atmos_pres, cl_1, cl)

trollc <- importNOAA(code = "010887-99999", hourly = TRUE, year = 2018:2019)
trollc <- select(trollc, station, date, latitude, longitude, atmos_pres, cl_1, cl)

trolla <- importNOAA(code = "010877-99999", hourly = TRUE, year = 2018:2019)
trolla <- select(trolla, station, date, latitude, longitude, atmos_pres, cl_1, cl)

utsirafyr <- importNOAA(code = "014030-99999", hourly = TRUE, year = 2018:2019)
utsirafyr <- select(utsirafyr, station, date, latitude, longitude, atmos_pres, cl_1, cl)

lista <- importNOAA(code = "014270-99999", hourly = TRUE, year = 2018:2019)
lista <- select(lista, station, date, latitude, longitude, atmos_pres, cl_1, cl)

lindesnes <- importNOAA(code = "014360-99999", hourly = TRUE, year = 2018:2019)
lindesnes <- select(lindesnes, station, date, latitude, longitude, atmos_pres, cl_1, cl)

marnock <- importNOAA(code = "031407-99999", hourly = TRUE, year = 2018:2019)
marnock <- select(marnock, station, date, latitude, longitude, atmos_pres, cl_1, cl)

mungo <- importNOAA(code = "031406-99999", hourly = TRUE, year = 2018:2019)
mungo <- select(mungo, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ekofisk <- importNOAA(code = "014000-99999", hourly = TRUE, year = 2018:2019)
ekofisk <- select(ekofisk, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ekofiskoil <- importNOAA(code = "014033-99999", hourly = TRUE, year = 2018:2019)
ekofiskoil <- select(ekofiskoil, station, date, latitude, longitude, atmos_pres, cl_1, cl)

a12 <- importNOAA(code = "062050-99999", hourly = TRUE, year = 2018:2019)
a12 <- select(a12, station, date, latitude, longitude, atmos_pres, cl_1, cl)

harald <- importNOAA(code = "060224-99999", hourly = TRUE, year = 2018:2019)
harald <- select(harald, station, date, latitude, longitude, atmos_pres, cl_1, cl)

tyrae <- importNOAA(code = "060223-99999", hourly = TRUE, year = 2018:2019)
tyrae <- select(tyrae, station, date, latitude, longitude, atmos_pres, cl_1, cl)

gormc <- importNOAA(code = "060221-99999", hourly = TRUE, year = 2018:2019)
gormc <- select(gormc, station, date, latitude, longitude, atmos_pres, cl_1, cl)

halfdan <- importNOAA(code = "060222-99999", hourly = TRUE, year = 2018:2019)
halfdan <- select(halfdan, station, date, latitude, longitude, atmos_pres, cl_1, cl)

thyboroen <- importNOAA(code = "060520-99999", hourly = TRUE, year = 2018:2019)
thyboroen <- select(thyboroen, station, date, latitude, longitude, atmos_pres, cl_1, cl)

hvide <- importNOAA(code = "060580-99999", hourly = TRUE, year = 2018:2019)
hvide <- select(hvide, station, date, latitude, longitude, atmos_pres, cl_1, cl)

hornsb <- importNOAA(code = "060170-99999", hourly = TRUE, year = 2018:2019)
hornsb <- select(hornsb, station, date, latitude, longitude, atmos_pres, cl_1, cl)

hornsa <- importNOAA(code = "060160-99999", hourly = TRUE, year = 2018:2019)
hornsa <- select(hornsa, station, date, latitude, longitude, atmos_pres, cl_1, cl)

westerland <- importNOAA(code = "100180-99999", hourly = TRUE, year = 2018:2019)
westerland <- select(westerland, station, date, latitude, longitude, atmos_pres, cl_1, cl)

nordholz <- importNOAA(code = "101360-99999", hourly = TRUE, year = 2018:2019)
nordholz <- select(nordholz, station, date, latitude, longitude, atmos_pres, cl_1, cl)

f3 <- importNOAA(code = "062390-99999", hourly = TRUE, year = 2018:2019)
f3 <- select(f3, station, date, latitude, longitude, atmos_pres, cl_1, cl)

j6 <- importNOAA(code = "062110-99999", hourly = TRUE, year = 2018:2019)
j6 <- select(j6, station, date, latitude, longitude, atmos_pres, cl_1, cl)

d15 <- importNOAA(code = "062010-99999", hourly = TRUE, year = 2018:2019)
d15 <- select(d15, station, date, latitude, longitude, atmos_pres, cl_1, cl)

f16 <- importNOAA(code = "062060-99999", hourly = TRUE, year = 2018:2019)
f16 <- select(f16, station, date, latitude, longitude, atmos_pres, cl_1, cl)

l9 <- importNOAA(code = "062070-99999", hourly = TRUE, year = 2018:2019)
l9 <- select(l9, station, date, latitude, longitude, atmos_pres, cl_1, cl)

vlieland <- importNOAA(code = "062420-99999", hourly = TRUE, year = 2018:2019)
vlieland <- select(vlieland, station, date, latitude, longitude, atmos_pres, cl_1, cl)

helipad <- importNOAA(code = "062120-99999", hourly = TRUE, year = 2018:2019)
helipad <- select(helipad, station, date, latitude, longitude, atmos_pres, cl_1, cl)

k14 <- importNOAA(code = "062040-99999", hourly = TRUE, year = 2018:2019)
k14 <- select(k14, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ruyter <- importNOAA(code = "062030-99999", hourly = TRUE, year = 2018:2019)
ruyter <- select(ruyter, station, date, latitude, longitude, atmos_pres, cl_1, cl)

zeebrugge <- importNOAA(code = "064180-99999", hourly = TRUE, year = 2018:2019)
zeebrugge <- select(zeebrugge, station, date, latitude, longitude, atmos_pres, cl_1, cl)

oostende <- importNOAA(code = "064070-99999", hourly = TRUE, year = 2018:2019)
oostende <- select(oostende, station, date, latitude, longitude, atmos_pres, cl_1, cl)

landwick <- importNOAA(code = "036930-99999", hourly = TRUE, year = 2018:2019)
landwick <- select(landwick, station, date, latitude, longitude, atmos_pres, cl_1, cl)

koksijde <- importNOAA(code = "064070-99999", hourly = TRUE, year = 2018:2019)
koksijde <- select(koksijde, station, date, latitude, longitude, atmos_pres, cl_1, cl)

dunkerque <- importNOAA(code = "070100-99999", hourly = TRUE, year = 2018:2019)
dunkerque <- select(dunkerque, station, date, latitude, longitude, atmos_pres, cl_1, cl)

boulogne <- importNOAA(code = "070020-99999", hourly = TRUE, year = 2018:2019)
boulogne <- select(boulogne, station, date, latitude, longitude, atmos_pres, cl_1, cl)

touqet <- importNOAA(code = "070030-99999", hourly = TRUE, year = 2018:2019)
touqet <- select(touqet, station, date, latitude, longitude, atmos_pres, cl_1, cl)

lydd <- importNOAA(code = "038873-99999", hourly = TRUE, year = 2018:2019)
lydd <- select(lydd, station, date, latitude, longitude, atmos_pres, cl_1, cl)

dieppe <- importNOAA(code = "070400-99999", hourly = TRUE, year = 2018:2019)
dieppe <- select(dieppe, station, date, latitude, longitude, atmos_pres, cl_1, cl)

shoreham <- importNOAA(code = "038760-99999", hourly = TRUE, year = 2018:2019)
shoreham <- select(shoreham, station, date, latitude, longitude, atmos_pres, cl_1, cl)

bessin <- importNOAA(code = "070400-99999", hourly = TRUE, year = 2018:2019)
bessin <- select(dieppe, station, date, latitude, longitude, atmos_pres, cl_1, cl)

haumet <- importNOAA(code = "070220-99999", hourly = TRUE, year = 2018:2019)
haumet <- select(haumet, station, date, latitude, longitude, atmos_pres, cl_1)
haumet$cl <- haumet$cl_1

hague <- importNOAA(code = "070200-99999", hourly = TRUE, year = 2018:2019)
hague <- select(hague, station, date, latitude, longitude, atmos_pres, cl_1, cl)

alderney <- importNOAA(code = "038915-99999", hourly = TRUE, year = 2018:2019)
alderney <- select(alderney, station, date, latitude, longitude, atmos_pres, cl_1, cl)

carteret <- importNOAA(code = "070340-99999", hourly = TRUE, year = 2018:2019)
carteret <- select(carteret, station, date, latitude, longitude, atmos_pres, cl_1, cl)

portland <- importNOAA(code = "038570-99999", hourly = TRUE, year = 2018:2019)
portland <- select(portland, station, date, latitude, longitude, atmos_pres, cl_1, cl)

guernsey <- importNOAA(code = "038940-99999", hourly = TRUE, year = 2018:2019)
guernsey <- select(guernsey, station, date, latitude, longitude, atmos_pres, cl_1, cl)

jersey <- importNOAA(code = "038950-99999", hourly = TRUE, year = 2018:2019)
jersey <- select(jersey, station, date, latitude, longitude, atmos_pres, cl_1, cl)

plymouth <- importNOAA(code = "038270-99999", hourly = TRUE, year = 2018:2019)
plymouth <- select(plymouth, station, date, latitude, longitude, atmos_pres, cl_1, cl)

brehat <- importNOAA(code = "071210-99999", hourly = TRUE, year = 2018:2019)
brehat <- select(brehat, station, date, latitude, longitude, atmos_pres, cl_1, cl)

armor <- importNOAA(code = "071200-99999", hourly = TRUE, year = 2018:2019)
armor <- select(armor, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ploumanach <- importNOAA(code = "071170-99999", hourly = TRUE, year = 2018:2019)
ploumanach <- select(ploumanach, station, date, latitude, longitude, atmos_pres, cl_1, cl)

culdrose <- importNOAA(code = "038090-99999", hourly = TRUE, year = 2018:2019)
culdrose <- select(culdrose, station, date, latitude, longitude, atmos_pres, cl_1, cl)

batz <- importNOAA(code = "071160-99999", hourly = TRUE, year = 2018:2019)
batz <- select(batz, station, date, latitude, longitude, atmos_pres, cl_1, cl)

brignogan <- importNOAA(code = "071070-99999", hourly = TRUE, year = 2018:2019)
brignogan <- select(brignogan, station, date, latitude, longitude, atmos_pres, cl_1, cl)

ouessant <- importNOAA(code = "071000-99999", hourly = TRUE, year = 2018:2019)
ouessant <- select(ouessant, station, date, latitude, longitude, atmos_pres, cl_1, cl)

landsend <- importNOAA(code = "038060-99999", hourly = TRUE, year = 2018:2019)
landsend <- select(landsend, station, date, latitude, longitude, atmos_pres, cl_1, cl)

scilly <- importNOAA(code = "038030-99999", hourly = TRUE, year = 2018:2019)
scilly <- select(scilly, station, date, latitude, longitude, atmos_pres, cl_1, cl)

mathieu <- importNOAA(code = "071020-99999", hourly = TRUE, year = 2018:2019)
mathieu <- select(mathieu, station, date, latitude, longitude, atmos_pres, cl_1, cl)

raz <- importNOAA(code = "071030-99999", hourly = TRUE, year = 2018:2019)
raz <- select(raz, station, date, latitude, longitude, atmos_pres, cl_1, cl)

penmarch <- importNOAA(code = "072000-99999", hourly = TRUE, year = 2018:2019)
penmarch <- select(penmarch, station, date, latitude, longitude, atmos_pres, cl_1, cl)

melen <- importNOAA(code = "072030-99999", hourly = TRUE, year = 2018:2019)
melen <- select(melen, station, date, latitude, longitude, atmos_pres, cl_1, cl)


# 2. Combine datasets ####
noaa <- do.call("rbind", list(andrew, sleipner, lerwick, gullfax, bruce, heimdal, harding, ula, trollc, trolla, utsirafyr, lista, lindesnes, marnock, mungo, ekofisk, ekofiskoil, a12, gormc, harald, tyrae, thyboroen, halfdan, hvide, hornsb, hornsa, westerland, nordholz, f3, j6, d15, f16, l9, vlieland, helipad, ruyter, k14, zeebrugge, landwick, oostende, koksijde, dunkerque, boulogne, touqet, dieppe, lydd, shoreham, haumet, bessin, hague, alderney, carteret, portland, guernsey, jersey, plymouth, brehat, ploumanach, armor, culdrose, batz, brignogan, ouessant, landsend, scilly, mathieu, raz, penmarch, melen))

# Remove location-specific datasets
rm(andrew, sleipner, lerwick, gullfax, bruce, heimdal, harding, ula, trollc, trolla, utsirafyr, lista, lindesnes, marnock, mungo, ekofisk, ekofiskoil, a12, gormc, harald, tyrae, thyboroen, halfdan, hvide, hornsb, hornsa, westerland, nordholz, f3, j6, d15, f16, l9, vlieland, helipad, ruyter, k14, zeebrugge, landwick, oostende, koksijde, dunkerque, boulogne, touqet, dieppe, lydd, shoreham, haumet, bessin, hague, alderney, carteret, portland, guernsey, jersey, plymouth, brehat, ploumanach, armor, culdrose, batz, brignogan, ouessant, landsend, scilly, mathieu, raz, penmarch, melen)



# 3. Load dataset with all eels ####
data <- read.csv("./data/interim/data_circadian_tidal.csv")
data$X <- NULL
data$ID <- factor(data$ID)


# Filter 1 animal for testing
subset <- filter(data, ID == "16031")
summary(subset$direction)

# Check NAs
sum(is.na(noaa$atmos_pres))
sum(is.na(noaa$cl_1))  
sum(is.na(noaa$cl))  
sum(is.na(noaa$latitude))  
sum(is.na(noaa$longitude))  

# Remove unnecessary columns
noaa$atmos_pres <- NULL
noaa$cl_1 <- NULL

# Remove NAs 
noaa <- noaa[!is.na(noaa$cl), ]




# 4. Link environmental NOAA data to DST tracks

# create GIS layers from the tracks and NOAA stations
det_stations <- st_as_sf(subset,
                         coords = c("avg_lon",
                                    "avg_lat"),
                         crs = 4326)

env_stations <- st_as_sf(noaa,
                         coords = c("longitude",
                                    "latitude"),
                         crs = 4326)




