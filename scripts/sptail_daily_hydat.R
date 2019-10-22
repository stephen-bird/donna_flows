library("here")
library("tidyhydat")
library("purrr")
library("corrr")
library("igraph")
library("ggraph")
library("ggmap")
library("dplyr")
library("sf")
library("stringr")
library("ggrepel")
library("raster")
library("xlsx")

# Create a point for Donna Creek -----------------------------------------------

donna_pt <- tibble(x = 424242, y = 6157082, loc = "Donna")
donna_pt <- st_as_sf(donna_pt,
                     coords = c("x", "y"),
                     crs = 26910)
donna_pt <- st_transform(donna_pt, 3005) # Reproject to Albers


# Import Hydrologic Zone map of BC ---------------------------------------------

hydro_zones <- st_read("data/raw/HYDZ_HYDROLOGICZONE_SP/HYD_BC_H_Z_polygon.shp")

# Plot the final map and observe that Donna Creek is in zone 8, Northern Thompson Plateau
plot(st_geometry(hydro_zones), col = sf.colors(29, categorical = TRUE), border = 'grey', 
     axes = TRUE)
text(st_coordinates(st_centroid(hydro_zones)), labels = hydro_zones$HYDZN_NO) # Hydro zones
text(st_coordinates(st_centroid(donna_pt)), labels = c("."), col = "red",cex = 3) # Location of Donna Creek


# Load the Hydat data and subset as needed -------------------------------------

# Run this line if the HyDat database has not been downwlaoded locally:
# download_hydat()
hy_dir() # Show directory where database is located
hy_bc <- hy_stations(prov_terr_state_loc = c("BC")) # Filter by BC
# Create a sf object:
hy_bc <- st_as_sf(hy_bc,
                 coords = c("LONGITUDE","LATITUDE"),
                 crs = 4326)
# Reproject from WGS to Albers
hy_bc <- st_transform(hy_bc, 3005)
# Plot all BC stations:
plot(st_geometry(hy_bc), col = sf.colors(29, categorical = TRUE), border = 'grey', 
     axes = TRUE)
# Plot the hydrologic zone map and location of all stations (black dots):
plot(st_geometry(hydro_zones), col = sf.colors(29, categorical = TRUE), border = 'grey', 
     axes = TRUE)
text(st_coordinates(st_centroid(donna_pt)), labels = c("."), col = "red",cex = 3) # Add location of Donna Creek
text(st_coordinates(st_centroid(hydro_zones)), labels = hydro_zones$HYDZN_NO) # Add the Hydro Zone labels
text(st_coordinates(st_centroid(hy_bc)), labels = c(".")) # Add the location of all BC hydrometric stations

# Filter by zone 8:
hydro_zones %>% filter(HYDZN_NO == 8) -> hydro_zone8
hydro_zone8 <- st_set_crs(hydro_zone8, 3005)
zone8_stations <- st_intersection(hy_bc, hydro_zone8) # Subset the hy_bc map by by Hydro Zone 8

# Convert to a df and then extract a vector with all Stations Numners in Zone 8:
zone8_stations_df <- as.data.frame(zone8_stations) # To a df
zone8_stations_df <- zone8_stations_df$STATION_NUMBER # Vector of Station Names

# Find the length of all station records in Zone 8:
no_stations <- length(zone8_stations_df) # Need the length of record for the loop below:
zone8_duration <- data.frame()

for(i in 1:no_stations) {
        # Extract the record length for each station in the zone of interest (note some stations may be listed more than once)
        hy_stn_data_range() %>%
                filter(STATION_NUMBER == zone8_stations_df[[i]])  -> one_station
        zone8_duration <- rbind(zone8_duration,one_station)
        zone8_duration
}


# Select all stations with at least 10 years of discharge data collected sometime between 1945 and 2012:
zone8_duration %>%
        filter(DATA_TYPE == "Q", Year_from <= 2003, Year_to >= 1954, RECORD_LENGTH >= 10) -> zone8_duration
# Summarize all record durations
summary(zone8_duration[,4:6])
length(zone8_duration$STATION_NUMBER)
# Subset the hy_bc tibble by our best station filter:
hy_bc %>% left_join(zone8_duration) -> hy_zone8
zone8_duration %>% left_join(hy_bc) -> hy_zone8
merge(hy_bc,zone8_duration) -> hy_zone8


# Summerize stations:
summary(hy_zone8$DRAINAGE_AREA_GROSS)
hist(hy_zone8$DRAINAGE_AREA_GROSS)


# Plot the hydrologic zone map and location of all stations (black dots):
plot(st_geometry(hydro_zones), col = sf.colors(29, categorical = TRUE), border = 'grey', 
     axes = TRUE)
text(st_coordinates(st_centroid(donna_pt)), labels = c("."), col = "red",cex = 3) # Add location of Donna Creek
text(st_coordinates(st_centroid(hydro_zones)), labels = hydro_zones$HYDZN_NO) # Add the Hydro Zone labels
text(st_coordinates(st_centroid(hy_zone8)), labels = c(".")) # Add the location of all BC hydrometric stations


# Plot the data ----------------------------------------------------------------



# Set the bounds of the map by the extebt of Hydro Zone 8:
hydro_zone8 <- st_transform(hydro_zone8, 4326)
map_extend <- st_bbox(hydro_zone8)
names(map_extend) <- c("left","bottom","right","top")

# Acquire the terrain map:
terrain_map <- get_map(map_extend,
                       source = "google", maptype = 'satellite', zoom = 9)

# Reproject Donna:
donna_pt <- st_transform(donna_pt, 4326)
donna_coord <- st_coordinates(donna_pt) # Extract coordinates to a tibble
donna_coord <- as.data.frame(donna_coord)
donna_label <- c("Donna Creek")
donna_coord <- cbind(donna_coord,donna_label) # Ready for ggmap

# Reproject hy_zone8:
hy_zone8_pt <- st_transform(hy_zone8, 4326)
hy_zone8_coord <- st_coordinates(hy_zone8_pt) # Extract coordinates to a tibble
hy_zone8_coord <- as.data.frame(hy_zone8_coord)
hy_zone8_label <- hy_zone8$STATION_NUMBER
hy_zone8_coord <- cbind(hy_zone8_coord,hy_zone8_label) # Ready for ggmap

# Plot the terrain map with the selected stations and Donna Creek:
ggmap(terrain_map) + 
        geom_point(data = donna_coord, aes(x = X, y = Y), color = 'red', size = 3) +
        geom_text(data = donna_coord, aes(x = X, y = Y, label = donna_label), vjust = -.4, hjust = -.1) +
        geom_point(data = hy_zone8_coord, aes(x = X, y = Y), color = 'blue', size = 1) +
        geom_label_repel(data = hy_zone8_coord, aes(x = X, y = Y, label = hy_zone8_label), vjust = -.4, hjust = -.1)

                  
# Retrieve all the daily records -----------------------------------------------


# Find the length of all station records in Zone 8:
no_stations <- length(hy_zone8_coord$X) # Need the length of record for the loop below:
daily_records <- list()

for(i in 1:no_stations) {
        # Extract the daily streamflow data for all selected station and save to a list:
        single_record <- (hy_daily(station_number = zone8_duration$STATION_NUMBER[i]))
        daily_records[[i]] <- single_record
        
}

# Output to a file
file_name <- paste(hy_zone8_label,".csv",sep="")

for(i in 1:no_stations) {
        # Extract the daily streamflow data for all selected station and save to a list:
        write.csv(daily_records[[i]], file = file_name[[i]])
        
}
