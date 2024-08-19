# Load the required packages:

library(sf)       # Simple Features data structures
library(units)
library(sp)        # spcosa uses `sp` data structures
library(rJava)     # required by spcosa
library(spcosa)    # the "spatial coverage sampling" package
library(ggplot2)   # graphics
library(tmap)
```
# Load the polygon (AOI) and transform it to EPSG:32718
poly_ws <- st_read("./data/basin_mantaro.gpkg")
poly_ws <- st_transform(poly_ws, 32718)

# Compute the area of the polygon
area_poly <- st_area(poly_ws)
area_poly

# Plot the polygon
map_ws <- tm_shape(poly_ws) +
    tm_polygons(fill = "grey", fill_alpha = 0.5, lwd = 4) +
    tm_graticules(lwd = 0.5, col = "grey", alpha = 0.5) +
    tm_compass(type = "8star", position = c("left", "top"))

map_ws

# Convert the polygon to sp for use with spcosa
poly_ws_sp <- as(poly_ws, "Spatial")

# Divide the polygon into strata
ws_strata <- stratify(poly_ws_sp, nStrata = 100, nTry = 10)

# Plot the strata
plot(ws_strata) +
    scale_x_continuous(name = "Easting (km)") +
    scale_y_continuous(name = "Northing (km)") +
    labs(title = "100 compact strata")

set.seed(123)

ws_sample1 <- spsample(ws_strata)
plot(ws_strata, ws_sample1) +
    scale_x_continuous(name = "Easting (km)") +
    scale_y_continuous(name = "Northing (km)") +
    labs(title = "100 compact strata, sample at centroids")

## Controlling the discretization

ws_strata_fine <- stratify(poly_ws_sp, nStrata = 100, nTry = 10, nGridCells = 12000)

ws_sample_fine <- spsample(ws_strata_fine)

plot(ws_strata_fine, ws_sample_fine) +
    scale_x_continuous(name = "Easting (km)") +
    scale_y_continuous(name = "Northing (km)") +
    labs(title = "100 compact strata - fine discretization")

## Stratified random sampling

ws_sample_stratified <- spsample(ws_strata_fine, n = 3)

plot(ws_strata_fine, ws_sample_stratified) +
    scale_x_continuous(name = "Easting (km)") +
    scale_y_continuous(name = "Northing (km)") +
    labs(title = "100 compact strata, 3 random points per stratum")

## Infill sampling

prior_points <- st_read("./data/prior_points_ws.gpkg")

prior_points_sp <- as(prior_points, "Spatial")

strata_infill <- stratify(poly_ws_sp, prior_points_sp,
    nStrata = 100, nTry = 10,
    nGridCells = 12000
)
new_points <- spsample(strata_infill)

plot(strata_infill, new_points) +
    scale_x_continuous(name = "Easting (km)") +
    scale_y_continuous(name = "Northing (km)") +
    labs(title = "100 compact strata, stations at centroid")


## Exporting the sampled points

infill.points.df <- as(new_points, "data.frame")

# convert to sf to change CRS
infill.pts.sf <- st_as_sf(infill.points.df, coords = c("x1", "x2"))

st_crs(infill.pts.sf) <- st_crs(poly_ws_sp)

infill.pts.sf <- st_transform(infill.pts.sf, 4326)
#st_write(infill.pts.sf, "./data/infill_points_geo.gpkg")

# convert back to data.frame for export
infill.pts.df <- as.data.frame(st_coordinates(infill.pts.sf))
names(infill.pts.df) <- c("Long", "Lat")

library(readr)
write_csv(infill.pts.df, "./data/infill_points.csv")

