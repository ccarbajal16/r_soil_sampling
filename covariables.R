#' Terrain Analysis Script for Soil Sampling
#' 
#' This script performs terrain analysis including calculation of slope, aspect,
#' TPI, TRI, and accumulated cost surfaces using a digital elevation model.
#'
#' @author Revised version
#' @date 2025-03-12

# Check and install required packages if needed
required_packages <- c("terra", "gdistance", "elevatr", "sf")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cran.r-project.org")
}

# Load required packages
suppressPackageStartupMessages({
  library(terra)
  library(gdistance)
  library(elevatr)
  library(sf)
})

#' Error handling function with more detailed output
#' 
#' @param expr An expression to be evaluated
#' @param msg An optional custom message to prefix error messages
#' @return The result of expr or NULL if an error occurs
handle_error <- function(expr, msg = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      prefix <- if(!is.null(msg)) paste0(msg, ": ") else "Error: "
      message(prefix, e$message)
      return(NULL)
    },
    warning = function(w) {
      prefix <- if(!is.null(msg)) paste0(msg, " Warning: ") else "Warning: "
      message(prefix, w$message)
      # Continue with the computation despite warning
      return(expr)
    }
  )
}


#' Clean up memory
#' 
#' @param objects Objects to remove, NULL to use garbage collection only
#' @return NULL (invisibly)
cleanup <- function(objects = NULL) {
  if (!is.null(objects)) {
    rm(list = objects, envir = parent.frame())
  }
  gc(verbose = FALSE)
  invisible(NULL)
}

#' Main function to execute terrain analysis
#' 
#' @param aoi_path Path to the area of interest GPKG file
#' @param output_dir Directory to save output files
#' @param start_point Coordinates for accumulated cost calculation
#' @param dem_z Zoom level for DEM download (1-14)
#' @param dem_src Source for DEM data ("aws", "glo-30", etc.)
#' @return Invisibly returns TRUE if successful
main <- function(
  aoi_path = "data/basin_mantaro.gpkg",
  output_dir = "data",
  start_point = c(x = -75.4447, y = -11.7356),
  dem_z = 10, 
  dem_src = "aws"
) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    show_progress(paste("Created output directory:", output_dir))
  }
  
  show_progress("Starting terrain analysis...")
  
  # Check if AOI file exists
  if (!file.exists(aoi_path)) {
    stop("AOI file not found: ", aoi_path)
  }
  
  # Get layer names
  layer_names <- st_layers(aoi_path)$name
  if (length(layer_names) == 0) {
    stop("No layers found in AOI file")
  }
  
  # Use first layer if basin_mantaro doesn't exist
  aoi_layer <- if ("basin_mantaro" %in% layer_names) "basin_mantaro" else layer_names[1]
  show_progress(paste("Using layer:", aoi_layer))
  
  # Read AOI
  pol_sf <- handle_error(
    st_read(dsn = aoi_path, layer = aoi_layer, quiet = TRUE),
    "Failed to read AOI file"
  )
  if (is.null(pol_sf)) stop("Failed to read AOI file")
  
  # Create file paths
  dem_path <- file.path(output_dir, "dem_basin.tif")
  
  # Check if DEM already exists to avoid redownloading
  if (file.exists(dem_path)) {
    show_progress("DEM already exists. Using existing file.")
  } else {
    # Download and save DEM
    show_progress("Downloading DEM data...")
    dem <- handle_error(
      get_elev_raster(pol_sf, z = dem_z, src = dem_src, clip = "locations"),
      "Failed to download DEM"
    )
    if (is.null(dem)) stop("Failed to download DEM")
    
    # Convert raster to SpatRaster for terra compatibility
    dem_terra <- rast(dem)
    
    # Save DEM
    show_progress("Saving DEM...")
    writeRaster(dem_terra, dem_path, overwrite = TRUE)
    
    # Clean up
    cleanup(c("dem", "dem_terra"))
  }
  
  # Load DEM as SpatRaster
  show_progress("Processing terrain variables...")
  dem <- rast(dem_path)
  
  # Define terrain variables with configuration
  terrain_vars <- list(
    slope = list(name = "slope", unit = "radians", description = "Slope steepness"),
    aspect = list(name = "aspect", unit = "radians", description = "Slope direction"),
    tpi = list(name = "TPI", unit = NULL, description = "Topographic Position Index"),
    tri = list(name = "TRI", unit = NULL, description = "Terrain Ruggedness Index")
  )
  
  # Calculate and save terrain variables
  for (var in names(terrain_vars)) {
    output_file <- file.path(output_dir, paste0(var, "_basin.tif"))
    
    # Skip if output file already exists
    if (file.exists(output_file)) {
      show_progress(paste(var, "already exists. Skipping calculation."))
      next
    }
    
    show_progress(paste("Calculating", terrain_vars[[var]]$description, "(" , var, ")"))
    
    # Calculate terrain variable
    result <- handle_error(
      terrain(
        dem, 
        v = terrain_vars[[var]]$name, 
        unit = terrain_vars[[var]]$unit
      ),
      paste("Failed to calculate", var)
    )
    
    if (!is.null(result)) {
      # Save result
      writeRaster(result, output_file, overwrite = TRUE)
      show_progress(paste("Saved", var, "to", output_file))
      
      # Clean up
      cleanup("result")
    }
  }
  
  # Calculate accumulated cost surface
  cost_output <- file.path(output_dir, "cost_basin.tif")
  
  if (file.exists(cost_output)) {
    show_progress("Cost surface already exists. Skipping calculation.")
  } else {
    show_progress("Calculating accumulated cost surface...")
    
    # Convert terra raster to raster object for gdistance compatibility
    dem_r <- handle_error(
      raster(dem_path),
      "Failed to convert DEM to raster format"
    )
    if (is.null(dem_r)) stop("Failed to load DEM as raster")
    
    # Create transition layer
    show_progress("Creating transition layer...")
    trans <- handle_error(
      transition(dem_r, transitionFunction = mean, directions = 8),
      "Failed to create transition layer"
    )
    if (is.null(trans)) stop("Failed to create transition layer")
    
    # Geographically correct the transition layer
    show_progress("Applying geographic correction...")
    cost_transition <- handle_error(
      geoCorrection(trans),
      "Failed to apply geographic correction"
    )
    if (is.null(cost_transition)) stop("Failed to create cost transition layer")
    
    # Calculate accumulated cost
    show_progress("Calculating accumulated cost...")
    accumulated_cost <- handle_error(
      accCost(cost_transition, start_point),
      "Failed to calculate accumulated cost"
    )
    if (is.null(accumulated_cost)) stop("Failed to calculate accumulated cost")
    
    # Save result
    writeRaster(accumulated_cost, cost_output, overwrite = TRUE)
    show_progress(paste("Saved cost surface to", cost_output))
    
    # Clean up
    cleanup(c("dem_r", "trans", "cost_transition", "accumulated_cost"))
  }
  
  show_progress("Terrain analysis completed successfully!")
  invisible(TRUE)
}

# Execute main function with error handling if script is run directly
if (sys.nframe() == 0) {
  handle_error(main())
}
