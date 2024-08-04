# Load required packages
library(terra)
library(sf)
library(sp)
library(clhs)
library(raster)
library(tidyverse)

# List of raster file names
files_raster <- c("aspect_basin.tif",  "cost_basin.tif", "dem_basin.tif", 
                "slope_basin.tif", "tpi_basin.tif", "tri_basin.tif")

# Create a stack of rasters
r.stack <- rast(paste0("data/", files_raster))

# Assign names to the layers of the stack
names(r.stack) <- c('aspect', 'cost', 'dem', 'slope', 'tpi', 'tri')

# Create a regular grid of points on top of our raster stack
set.seed(5)
s <- spatSample(r.stack, size= 1000, method = "regular", xy = TRUE)

# Filter out the points with missing values in the 'cost' attribute
s <- s[!is.na(s$cost), ]

# Create a spatial points data frame
s_spdf <- SpatialPointsDataFrame(coords = s[, 1:2], data = s[, 3:ncol(s)])

# The cost surface is activated through the 'cost' argument
s.clhs <- clhs(s_spdf, size = 60, progress = FALSE, iter = 2000, cost = 'cost', simple = FALSE)

# Extract the indices
subset.idx <- s.clhs$index_samples

# Visualize the DEM and cost layers, with the selected points marked in red
terra::plot(r.stack$dem, axes=TRUE, legend=TRUE)
terra::contour(r.stack$cost, nlevels=10, col='#464343', add=TRUE)
points(s[subset.idx, ], col = '#FF00C8', pch=16, cex = 1.5)

# Save the selected points as a CSV 
st_write(s[subset.idx, ], "outputs/clhs_points_basin.csv")

# Filter selected points
points <- read_csv("outputs/clhs_points_basin.csv")

points_filtered <- points |>
    mutate(accessibility = case_when(
        cost > 1.5 ~ "less accessible",
        cost <= 1.5 ~ "more accessible"
    ), ID = seq(1, length(points$cost), 1)) |> 
    select (ID,x, y, dem, cost, accessibility)

# Save the filtered points as a CSV 
st_write(points_filtered, "outputs/clhs_points_filter.csv")

### Similarity Buffer analysis ###

# Convert terra rasters to data.frame
df_raster <- as.data.frame(r.stack, xy = TRUE)

# Convert data frame to SpatialPointsDataFrame
coordinates(df_raster) <- c("x", "y")

# Load the raster layer
rast_layer <- raster("data/nasadem_basin.tif")

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
gw_cuenca <- similarity_buffer(r.stack, s.clhs$sampled_data, buffer = 500)

# Save the buffer results as a GeoTIFF named 'list_buffer.tif' in the 'outputs_mantaro' directory
terra::writeRaster(gw_cuenca, "outputs/grids_buffer.tif")