# Load required packages
library(terra)
library(gdistance)
library(elevatr)
library(sf)

# Define your area of interest (AOI)
pol_sf <- st_read(
  dsn = "data/basin_mantaro.gpkg",
  layer = "basin_mantaro"
)

# Download the DEM data
dem <- get_elev_raster(pol_sf, z = 10, src = "aws", clip = "locations")

# Access API key from opentopography account and then run: set_opentopo_key()
#dem_srtm <- get_elev_raster(pol_sf, src = "gl1", clip = "locations")
#dem_alos <- get_elev_raster(pol_sf, src = "alos", clip = "locations")

writeRaster(dem, "data/dem_basin.tif", overwrite = TRUE)

# Create a raster object with terra package
dem <- rast("data/dem_basin.tif")

# Calculate slope
slope <- terrain(dem, v = "slope", unit = "radians") 

# Calculate aspect
aspect <- terrain(dem, v = "aspect", unit = "radians") 

# Calculate TPI (Topographic Position Index)
tpi <- terrain(dem, v = "TPI")

# Calculate TRI (Terrain Ruggeedness Index)
tri <- terrain(dem, v = "TRI")

# Save the derived layers
writeRaster(slope, filename = "data/slope_basin.tif", overwrite = TRUE)
writeRaster(aspect, filename = "data/aspect_basin.tif", overwrite = TRUE)
writeRaster(tpi, filename = "data/tpi_basin.tif", overwrite = TRUE)
writeRaster(tri, filename = "data/tri_basin.tif", overwrite = TRUE)


# Calculate the accumulated cost surface layer

library(raster)

dem_r <- raster("data/dem_basin.tif")

# Create a transition layer based on the cost raster
cost_transition <- transition(dem_r, transitionFunction = mean, directions = 8)

# Correct the transition layer for accurate distance calculations
cost_transition <- geoCorrection(cost_transition)

# Define the start point coordinates
start_point <- c(x = -75.4447, y = -11.7356)

# Accumulated cost surface
accumulated_cost <- accCost(cost_transition, start_point)

# Export raster
writeRaster(accumulated_cost, "data/cost_basin.tif", overwrite = TRUE)

