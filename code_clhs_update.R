# Load required packages
library(terra)
library(sf)
library(sp)
library(clhs)
library(raster)

# List of raster file names
files_raster <- c("Aspect_mantaro.tif",  "Costo_acumulado_mantaro.tif", "nasadem_altomantaro.tif", 
                "Slope_mantaro.tif", "tmed_mantaro_down.tif", "TPI_mantaro.tif", "TSAVI_mantaro.tif")

# Create a stack of rasters
r.stack <- rast(paste0("data/", files_raster))

# Assign names to the layers of the stack
names(r.stack) <- c('aspect', 'cost', 'dem', 'slope', 'tmed', 'tpi', 'tsavi')

# Create a regular grid of points on top of our raster stack
set.seed(5)
s <- spatSample(r.stack, size= 12000, method = "regular", xy = TRUE)

# Filter out the points with missing values in the 'cost' attribute
s <- s[!is.na(s$cost), ]

# Create a spatial points data frame
s_spdf <- SpatialPointsDataFrame(coords = s[, 1:2], data = s[, 3:ncol(s)])

# The cost surface is activated through the 'cost' argument
s.clhs <- clhs(s_spdf, size = 120, progress = FALSE, iter = 2000, cost = 'cost', simple = FALSE)

# Extract the indices
subset.idx <- s.clhs$index_samples

# Visualize the DEM and cost layers, with the selected points marked in red
terra::plot(r.stack$dem, axes=FALSE)
terra::contour(r.stack$cost, nlevels=10, col='#1a1818', add=TRUE)
points(s[subset.idx, ], col = 'red', pch=21)

# Save the selected points as an ESRI Shapefile named 'clhs_mantaro_points' in the 'outputs_mantaro' directory
sf::st_write(s[subset.idx, ], "outputs/clhs_mantaro_points.shp")


### Similarity Buffer analysis ###

# Convert terra rasters to data.frame
df_raster <- as.data.frame(r.stack, xy = TRUE)

# Convert data frame to SpatialPointsDataFrame
coordinates(df_raster) <- c("x", "y")

# Load the raster layer
rast_layer <- raster("data/nasadem_altomantaro.tif")

# Create a raster template
r <- raster(extent(rast_layer), res = 0.0002777778, crs = crs(rast_layer)) 

# Create a raster stack using a loop
r.stack <- stack() # Initialize an empty raster stack

# Loop through each variable in the SpatialPointsDataFrame
for (i in 1:ncol(df_raster@data)) {
  # Rasterize each variable
  layer <- rasterize(df_raster, r, df_raster@data[, i], fun = mean)  # Use mean for aggregation
  names(layer) <- names(df_raster@data)[i]
  r.stack <- stack(r.stack, layer)
}

# Calculate the Gower similarity index between the raster stack and the sampled data from the CLHS analysis, with a buffer of 250
gw_cuenca <- similarity_buffer(r.stack, s.clhs$sampled_data, buffer = 250)

# Save the buffer results as a GeoTIFF named 'list_buffer.tif' in the 'outputs_mantaro' directory
terra::writeRaster(gw_cuenca, "outputs/list_buffer.tif")