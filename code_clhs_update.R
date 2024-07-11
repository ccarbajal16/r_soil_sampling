# Load required packages
library(terra)
library(sf)
library(sp)
library(clhs)

# List of raster file names
files_raster <- c("Profile curvature_mantaro.tif", "Plan Curvature_mantaro.tif", "altura_standar_mantaro.tif", 
                  "Aspect_mantaro.tif",  "nasadem_altomantaro.tif", "prof_valle_mantaro.tif", "Slope_mantaro.tif", 
                  "TSAVI_mantaro.tif", "TPI_mantaro.tif", "twi_mantaro.tif", "tmed_mantaro_down.tif", 
                  "lst_2021_mean_alto_mantaro_geo.tif", "Costo_acumulado_mantaro.tif")

# Create a stack of rasters directly within the loop
r.stack <- terra::rast(lapply(files_raster, function(archivo) rast(paste("data/", archivo, sep = ""))))

# Assign names to the layers of the stack
names(r.stack) <- c('curvpr', 'curvpl', 'hstand', 'aspect', 'dem', 'profund', 'slope', 'tavi', 'tpi', 'twi', 'tepm', 'lst', 'cost')


# Create a regular grid of points on top of our raster stack
set.seed(5)
s <- terra::init(r.stack, fun= runif)

# Filter out the points with missing values in the 'cost' attribute
s <- s[!is.na(s$cost), ]

# Convert the matrix to a spatial object
s_sf <- st_as_sf(s, coords = c("x", "y"))

# Convert the spatial object to a SpatialPointsDataFrame
s_spdf <- as(s_sf, "Spatial")

# Now you have s_spdf as a SpatialPointsDataFrame

# The cost surface is activated through the 'cost' argument
s.clhs <- clhs(s, size = 120, progress = FALSE, iter = 2000, cost = 'cost', simple = FALSE)

# Extract the indices
subset.idx <- s.clhs$index_samples

# Visualize the DEM and cost layers, with the selected points marked in red
terra::plot(r.stack$dem, axes=FALSE)
terra::contour(r.stack$cost, nlevels=10, col='black', add=TRUE)
points(s[subset.idx, ], col = 'red', pch=21)

# Save the selected points as an ESRI Shapefile named 'clhs_mantaro_points' in the 'outputs_mantaro' directory
sf::st_write(s[subset.idx, ], "outputs/clhs_mantaro_points.shp")

# Calculate the Gower similarity index between the raster stack and the sampled data from the CLHS analysis, with a buffer of 250
gw_cuenca <- similarity_buffer(r.stack, s.clhs$sampled_data, buffer = 250)

# Save the buffer results as a GeoTIFF named 'list_buffer.tif' in the 'outputs_mantaro' directory
terra::writeRaster(gw_cuenca, "outputs_mantaro/list_buffer.tif")