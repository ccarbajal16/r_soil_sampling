# Load required packages
library(terra)
library(gdistance)
library(elevatr)
library(sf)

# Error handling function
handle_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    }
  )
}

# Progress message function
show_progress <- function(msg) {
  message(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg))
}

main <- function() {
  show_progress("Starting terrain analysis...")
  
  # Define area of interest (AOI)
  aoi_path <- "data/basin_mantaro.gpkg"
  if (!file.exists(aoi_path)) {
    stop("AOI file not found: ", aoi_path)
  }
  
  pol_sf <- handle_error(
    st_read(dsn = aoi_path, layer = "basin_mantaro", quiet = TRUE)
  )
  if (is.null(pol_sf)) stop("Failed to read AOI file")
  
  # Download and save DEM
  show_progress("Downloading DEM data...")
  dem <- handle_error(
    get_elev_raster(pol_sf, z = 10, src = "aws", clip = "locations")
  )
  if (is.null(dem)) stop("Failed to download DEM")
  
  dem_path <- "data/dem_basin.tif"
  writeRaster(dem, dem_path, overwrite = TRUE)
  rm(dem) # Clean up memory
  gc()
  
  # Process terrain variables using terra
  show_progress("Processing terrain variables...")
  dem <- rast(dem_path)
  
  # Calculate terrain variables
  terrain_vars <- list(
    slope = list(name = "slope", unit = "radians"),
    aspect = list(name = "aspect", unit = "radians"),
    tpi = list(name = "TPI", unit = NULL),
    tri = list(name = "TRI", unit = NULL)
  )
  
  for (var in names(terrain_vars)) {
    show_progress(paste("Calculating", var))
    result <- terrain(
      dem, 
      v = terrain_vars[[var]]$name, 
      unit = terrain_vars[[var]]$unit
    )
    writeRaster(
      result,
      filename = paste0("data/", var, "_basin.tif"),
      overwrite = TRUE
    )
    rm(result)
    gc()
  }
  
  # Calculate accumulated cost surface
  show_progress("Calculating accumulated cost surface...")
  dem_r <- raster(dem_path) # Convert to raster for gdistance compatibility
  
  # Create and correct transition layer
  cost_transition <- handle_error({
    trans <- transition(dem_r, transitionFunction = mean, directions = 8)
    geoCorrection(trans)
  })
  if (is.null(cost_transition)) stop("Failed to create cost transition layer")
  
  # Calculate accumulated cost
  start_point <- c(x = -75.4447, y = -11.7356)
  accumulated_cost <- handle_error(
    accCost(cost_transition, start_point)
  )
  if (is.null(accumulated_cost)) stop("Failed to calculate accumulated cost")
  
  writeRaster(accumulated_cost, "data/cost_basin.tif", overwrite = TRUE)
  
  show_progress("Terrain analysis completed successfully!")
  
  # Clean up
  rm(list = ls())
  gc()
}

# Execute main function with error handling
handle_error(main())
