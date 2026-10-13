library(sf)
library(ggplot2)
library(tmap)
library(sp)
library(raster)
library(dplyr)

#Importing data
districts <- st_read("C:/Users/Farida/Desktop/Geo_Ass_1/geoBoundaries-ETH-ADM2-all/geoBoundaries-ETH-ADM2_simplified.shx")
road_nt <- st_read("C:/Users/Farida/Desktop/Geo_Ass_1/ethiopia-latest-free.shp/gis_osm_roads_free_1.shx") 
electricity <- st_read("C:/Users/Farida/Desktop/Geo_Ass_1/ethiopia-electricity-transmission-network/Ethiopia Electricity Transmission Network.shx") 
popd<-raster("C:/Users/Farida/Desktop/Geo_Ass_1/eth_pd_2012_1km_ASCII_XYZ.csv")
st_drivers()
Powerplants <- st_read("C:/Users/Farida/Desktop/Geo_Ass_1/ETH_PowerPlants.shx") 
Villages <- st_read("C:/Users/Farida/Desktop/Geo_Ass_1/eth_capitaltowns-2021-shape-files/ETH_CapitalTowns/ETH_CapitalTowns.shx")

#Transforing To CRS target 

crs_target <- st_crs(districts)
print(crs_target)
electricity <- st_transform(electricity, crs_target)

road_nt <- st_transform(road_nt, crs_target)
primary_roads <- road_nt[road_nt$fclass == "secondary", ]

Powerplants <- st_set_crs(Powerplants, 4326)

Villages <-st_transform(Villages, crs_target)

#Dealing with Raster data 
# Crop and mask population density to the district boundaries
popd <- mask(crop(popd, districts), districts) 

# Convert raster to data frame for plotting
population_df <- as.data.frame(rasterToPoints(popd))  

# Convert raster to points
population_points <- rasterToPoints(popd, spatial = TRUE)

# Convert to sf object
population_sf <- st_as_sf(population_points)

# Convert population_df back to a raster object
population_raster <- rasterFromXYZ(population_df[, c("x", "y", "eth_pd_2012_1km_ASCII_XYZ")])

# the CRS of the raster 
crs(population_raster) <- st_crs(districts)$proj4string

#  spatial join and take the mean of the population density 
population_joined <- st_join(population_sf, districts)
colnames(population_joined)
colnames(districts)
population_density_aggregated <- population_joined %>%
  group_by(shapeID) %>%
  summarize(mean_density = mean(eth_pd_2012_1km_ASCII_XYZ, na.rm = TRUE))
population_density_aggregated_df <- as.data.frame(population_density_aggregated)

districts_filled <- districts %>%
  left_join(population_density_aggregated_df, by = "shapeID")


# Convert population_df back to a raster object
population_raster <- rasterFromXYZ(population_df[, c("x", "y", "eth_pd_2012_1km_ASCII_XYZ")])

# the CRS of the raster 
crs(population_raster) <- st_crs(districts)$proj4string


tmap_mode("view")

tm_shape(districts_filled) +
  tm_polygons(
    col = "mean_density",
    palette = c("#e6f0fa", "#c8dff1", "#98c6e4", "#6daed6", "#2171b5", "#08306b"),  # Custom blue palette
    style = "quantile",
    n = 6  
  ) +
  tm_shape(primary_roads) +
  tm_lines(col = "black", lwd = 1.5) +  
  tm_shape(electricity) +
  tm_lines(col = "red", lwd = 1) +  
  tm_shape(Powerplants) +  
  tm_dots(
    col = "red",  
    size = 0.01,  
    border.col = "black",  
    border.lwd = 0.5  
  ) +
  tm_shape(Villages) +  
  tm_dots(
    col = "yellow",  
    size = 0.009,  
    border.col = "white",  
    border.lwd = 0.5  
  ) +
  tm_layout(
    main.title = "Ethiopia: Population Density, Roads, Electricity Grid, Power Plants, and Villages",
    frame = FALSE, 
    legend.show = FALSE  
  )

