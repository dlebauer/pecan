#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##'
##' Convert ESRI shapefile (*.shp) to keyhole markup language (KML) file format
##'
##' @title Convert shapefile to KML
##'
##' @param dir Directory of GIS shapefiles to convert to kml/kmz
##' @param ext File extension for files to convert to kml/kmz.  Defaults to ESRI shapefile,
##' '.shp'.  [Place holder for other potential vector files to conver to kml]
##' @param kmz TRUE/FALSE. Option to write out file as a compressed kml. Requires zip utility
##' @param proj4 OPTIONAL. Define output proj4 projection string.  If set, input vector will be
##' reprojected to desired projection.  Not yet implemented.
##' @param color OPTIONAL. Fill color for output kml/kmz file
##' @param NameField OPTIONAL. Define names for individual features in KML/KMZ file
##' @param out.dir OPTIONAL. Output directory for converted files
##'
##' @export
##'
##' @examples
##' \dontrun{
##' dir <- Sys.glob(file.path(R.home(), 'library', 'PEcAn.data.land','data'))
##' out.dir <- path.expand('~/temp')
##' shp2kml(dir,'.shp',kmz=FALSE,NameField='STATE',out.dir=out.dir)
##' system(paste('rm -r ',out.dir))
##' }
##'
##' @author Shawn P. Serbin
shp2kml <- function(dir, ext, kmz = FALSE, proj4 = NULL, color = NULL, NameField = NULL, out.dir = NULL) {
  # TODO: Enable compression of KML files using zip/gzip utility.  Not quite figured this out yet
  # TODO: Allow assignment of output projection info by entering proj4 string
  # TODO: Allow for customization of output fill colors and line size
  # TODO: Allow for selection of taget attribute in output kml/kmz file(s)
  # TODO: Allow for setting out labels

  if (!is.null(out.dir)) {
    if (!file.exists(out.dir)) {
      dir.create(out.dir, recursive = TRUE)
    }
    output <- out.dir
  } else {
    output <- dir
  }

  # Get list of shapefiles in directory
  files <- list.files(path = dir, pattern = "*.shp", full.names = FALSE)
  remove <- grep("*xml", files)
  if (length(remove) > 0) {
    files <- files[-remove]
  }

  # loop here
  for (i in files) {
    print("")
    print(paste0("Converting : ** ", i, " ** to KML/KMZ file"))
    print("")
    print("")

    # Read in shapefile(s) & get coordinates/projection info shp.file <-
    # readShapeSpatial(file.path(dir,i),verbose=TRUE) coordinates(test) <- ~X+Y

    layers <- sf::st_layers(file.path(dir, i))
    # shp.file <- readOGR(file.path(dir,i),layer=layers) # no need to read in file

    # Display vector info to the console
    print("")
    print(paste0("Input layers: ", layers$name))
    print(paste0("Input projection info: ", layers$crs[[1]]$input))
    print("")

    # Write out kml/kmz using plotKML package if (is.null(color)){ color <- 'grey70' }

    if (kmz == TRUE) {
      # NOT YET FULLY IMPLEMENTED
      in.file <- file.path(dir, i, fsep = .Platform$file.sep)
      out.file <- file.path(output, unlist(strsplit(i, "\\.")), fsep = .Platform$file.sep)
      OGRstring <- paste0(
        "ogr2ogr -progress -f KML", " ",
        paste0(out.file, ".kmz"),
        " ", in.file, " ", "-dsco NameField=", NameField
      )
      system(OGRstring) # Run KML conversion

      # ADD COMPRESSION STEP HERE!!!
    } else {
      # kml(shp.file,file=paste(output,'test.kml'),colour = 'grey70', alpha = 0.75, width=2,
      # balloon=FALSE)
      # writeOGR(shp.file['STATE'],'test2.kml',layer='statep010',NameField='STATE',driver='KML')

      # Using ogr2ogr external system utility.  Works much better than R packages.
      in.file <- file.path(dir, i, fsep = .Platform$file.sep)
      out.file <- file.path(output, unlist(strsplit(i, "\\.")), fsep = .Platform$file.sep)
      OGRstring <- paste0(
        "ogr2ogr -progress -f KML", " ", paste0(out.file, ".kml"),
        " ", in.file, " ", "-dsco NameField=", NameField
      )
      system(OGRstring) # Run KML conversion
    }
  } # End of loop
} # shp2kml


#' Retrieve Attribute Information from Vector or Raster Layers
#'
#' This function extracts attribute information from a vector or raster file within a specified bounding box defined by coordinates.
#'
#' @param file Character. Path to the vector (e.g., Shapefile, GeoPackage) or raster (e.g., GeoTIFF) file.
#' @param coords Numeric vector. A vector containing `xmin`, `ymin`, `xmax`, `ymax` defining the bounding box for subsetting.
#'
#' @return
#' - For vector files: An `sf` object containing the subset of features within the bounding box.
#' - For raster files: A `SpatRaster` object cropped to the bounding box.
#'
#' @importFrom sf st_read st_bbox st_crop st_as_sfc
#' @importFrom terra rast crop ext
#'
#' @aliases get_attributes, get.attributes
#' @author Shawn P. Serbin
#' @author David LeBauer
#'
#' @examples
#' \dontrun{
#' # Example with a vector file (Shapefile)
#' vector_file <- "path/to/vector.shp"
#' bbox_coords <- c(-95, 42, -84, 47) # xmin, ymin, xmax, ymax
#' subset_vector <- get_attributes(file = vector_file, coords = bbox_coords)
#' print(subset_vector)
#'
#' # Example with a raster file (GeoTIFF)
#' raster_file <- "path/to/raster.tif"
#' subset_raster <- get_attributes(file = raster_file, coords = bbox_coords)
#' print(subset_raster)
#' }
get_attributes <- function(file, coords) {
  # Validate 'coords' input
  if (!is.numeric(coords) || length(coords) != 4) {
    stop("`coords` must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax).")
  }

  # Determine file type based on extension
  file_ext <- tools::file_ext(file)

  if (file_ext %in% c("shp", "gpkg", "geojson", "kml", "gml", "geo")) {
    # Handle vector data
    vector_sf <- sf::st_read(file, quiet = TRUE)

    # Create bounding box as sf object
    bbox_sf <- sf::st_as_sfc(sf::st_bbox(c(
      xmin = coords[1],
      ymin = coords[2],
      xmax = coords[3],
      ymax = coords[4]
    ), crs = sf::st_crs(vector_sf)))

    # Subset vector data within bounding box
    subset_vector <- sf::st_crop(vector_sf, bbox_sf)

    return(subset_vector)
  } else if (file_ext %in% c("tif", "tiff", "img", "grd")) {
    # Handle raster data
    raster_data <- terra::rast(file)

    # Define bounding box extent
    bbox_ext <- terra::ext(coords[1], coords[3], coords[2], coords[4])

    # Crop raster to bounding box
    subset_raster <- terra::crop(raster_data, bbox_ext)

    return(subset_raster)
  } else {
    stop("Unsupported file type. Please provide a vector (e.g., Shapefile, GeoPackage) or raster (e.g., GeoTIFF) file.")
  }
}


#--------------------------------------------------------------------------------------------------#
##'
##' Function to subset and clip a GIS vector or raster layer by a bounding box
##' or clip/subset layer (e.g. shapefile/KML)
##'
##' @param file input file to be subset
##' @param coords vector with xmin,ymin,xmax,ymax defing the bounding box for subset
##' @param sub.layer Vector layer defining the subset region
##' @param clip clip geometries to bounding box/subset layer? TRUE/FALSE
##' @param out.dir output directory for subset layer. Defaults to location of
##' input file.  Can also set to 'pwd'
##' @param out.name filename for subset layer.  Defaults to original filename with the suffix
##' *.sub
##' @examples
##' \dontrun{
##' # Test dataset
##' file <- Sys.glob(file.path(R.home(), 'library', 'PEcAn.data.land','data','*.shp'))
##' out.dir <- path.expand('~/temp')
##' # with clipping enabled
##' subset_layer(file=file,coords=c(-95,42,-84,47),clip=TRUE,out.dir=out.dir)
##' # without clipping enables
##' subset_layer(file=file,coords=c(-95,42,-84,47),out.dir=out.dir)
##' system(paste('rm -r',out.dir,sep=''))
##' }
##'
##' @export subset_layer
##'
##' @author Shawn P. Serbin
subset_layer <- function(file, coords = NULL, sub.layer = NULL, clip = FALSE, out.dir = NULL, out.name = NULL) {
  # Setup output directory for subset layer
  if (is.null(out.dir)) {
    out.dir <- dirname(file)
  } else if (out.dir == "pwd") {
    out.dir <- getwd()
  } else {
    out.dir <- out.dir
  }
  if (!file.exists(out.dir)) {
    dir.create(out.dir, recursive = TRUE)
  }

  # Setup output file name for subset layer
  if (is.null(out.name)) {
    out.name <- paste0(
      unlist(strsplit(basename(file), "\\."))[1],
      ".sub.", unlist(strsplit(basename(file), "\\."))[2]
    )
  } else {
    out.name <- out.name
  }

  print(paste0("Subsetting layer: ", out.name))
  output <- file.path(out.dir, out.name, fsep = .Platform$file.sep)

  if (unlist(strsplit(basename(file), "\\."))[2] == "kml") {
    format <- "-f KML"
  } else {
    format <- paste0("-f ", "'ESRI Shapefile'")
  }

  if (clip) {
    OGRstring <- paste0(
      "ogr2ogr -spat", " ", coords[1], " ", coords[2], " ", coords[3], " ", coords[4],
      " ", format, " ", output, " ", file, " ", "-clipsrc", " ", "spat_extent"
    )
  } else {
    OGRstring <- paste0(
      "ogr2ogr -spat", " ", coords[1], " ", coords[2], " ", coords[3], " ", coords[4],
      " ", format, " ", output, " ", file
    )
  }

  # Run subset command
  system(OGRstring)
} # subset_layer
#' Extract Raster Values at Point or Polygon Locations
#'
#' This function extracts raster values from a specified raster file at the locations of points or polygons provided either as a data frame with latitude and longitude or as an `sf` object with point or polygon geometries.
#'
#' @param raster_path Character. Path to the raster file.
#' @param spatial_data Input data. Either:
#'   - A data frame containing `lat` and `lon` columns (for points), or
#'   - An `sf` object with point or polygon geometries.
#' @param value_colname Character. Name of the new column to store extracted raster values. Default is `"raster_value"`.
#' @param scaling_factor Numeric. A factor to scale the extracted raster values. Default is `1` (no scaling).
#'
#' @return An `sf` object with an additional column containing the extracted (and scaled) raster values.
#'
#' @importFrom sf st_as_sf st_geometry_type st_crs st_transform st_read
#' @importFrom terra rast crs extract vect
#' @importFrom dplyr mutate
#' @author David LeBauer
#' @examples
#' \dontrun{
#' # Example with data frame (points)
#' points_df <- data.frame(
#'   id = 1:3,
#'   lon = c(-120.5, -121.3, -122.1),
#'   lat = c(35.3, 36.7, 34.8)
#' )
#' raster_values_df <- extract_raster_values(
#'   raster_path = "path/to/raster.tif",
#'   spatial_data = points_df,
#'   value_colname = "extracted_val",
#'   scaling_factor = 10
#' )
#'
#' # Example with sf object (polygons)
#' polygons_sf <- sf::st_read("path/to/polygons.shp")
#' raster_values_polygons <- extract_raster_values(
#'   raster_path = "path/to/raster.tif",
#'   spatial_data = polygons_sf,
#'   value_colname = "mean_raster_val",
#'   scaling_factor = 1
#' )
#' }
extract_raster_values <- function(raster_path, spatial_data, value_colname = "raster_value", scaling_factor = 1) {
  # Convert input to sf object and determine geometry type
  if (is.data.frame(spatial_data)) {
    if (!all(c("lat", "lon") %in% names(spatial_data))) {
      stop("The data frame must contain 'lat' and 'lon' columns.")
    }
    # Convert to sf object with POINT geometries
    spatial_sf <- sf::st_as_sf(spatial_data, coords = c("lon", "lat"), crs = 4326)
    geom_type <- "POINT"
  } else if (inherits(spatial_data, "sf")) {
    # Check geometry type
    geom_types <- unique(sf::st_geometry_type(spatial_data))
    if (all(geom_types %in% c("POINT", "MULTIPOINT"))) {
      geom_type <- "POINT"
      spatial_sf <- spatial_data
    } else if (all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      geom_type <- "POLYGON"
      spatial_sf <- spatial_data
    } else {
      stop("The `sf` object must contain either only POINT/MULTIPOINT or only POLYGON/MULTIPOLYGON geometries.")
    }
  } else {
    stop("`spatial_data` must be either a data frame with 'lat' and 'lon' columns or an `sf` object with point or polygon geometries.")
  }

  # Read the raster file using terra
  raster_data <- terra::rast(raster_path)

  # Check and align CRS
  raster_crs <- terra::crs(raster_data, describe = TRUE)$code
  spatial_crs <- sf::st_crs(spatial_sf)$epsg

  if (!is.na(raster_crs) && !is.na(spatial_crs) && raster_crs != spatial_crs) {
    spatial_sf <- sf::st_transform(spatial_sf, crs = raster_data)
  }

  # Extract raster values based on geometry type
  if (geom_type == "POINT") {
    extracted_data <- terra::extract(raster_data, terra::vect(spatial_sf))

    # Validate extraction result
    if (ncol(extracted_data) < 2) {
      stop("Extraction failed. Please check the raster and point locations.")
    }

    extracted_col <- extracted_data[[2]]
  } else if (geom_type == "POLYGON") {
    # For polygons, calculate summary statistics (e.g., mean)
    extracted_data <- terra::extract(raster_data, terra::vect(spatial_sf), fun = mean, na.rm = TRUE)

    # Validate extraction result
    if (ncol(extracted_data) < 2) {
      stop("Extraction failed. Please check the raster and polygon locations.")
    }

    extracted_col <- extracted_data[[2]]
  }

  # Add extracted (and scaled) raster values to the sf object and return
  spatial_sf <- spatial_sf |>
    dplyr::mutate(!!value_colname := extracted_col / scaling_factor)

  return(spatial_sf)
}
