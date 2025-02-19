#' Retrieve Cal-Adapt climate raster for a polygon or a point
#'
#' @description
#' Fetches climate raster data from Cal-Adapt for a specified polygon or point,
#' GCM, emission scenario, and time period. Supports LOCA (CMIP5-based) data,
#' and ensures that the geometry is within the bounding box of valid coordinates
#' for California.
#'
#' @references
#' - [Cal-Adapt](https://caladapt.org/)
#' - [Cal Adapt Data Catalog Documentation](https://berkeley-gif.github.io/caladapt-docs/data-catalog.html)
#' - [Cal-Adapt Loca Data Catalog](https://albers.cnr.berkeley.edu/data/scripps/loca/met/)
#'
#' @param sf_obj An `sf`, `sfc`, or `SpatRaster` row representing the points or polygon boundary
#'   (required for `download_caladapt_loca_raster()`).
#' @param var Character. Climate variable to retrieve. One of "tasmax", "tasmin", "pr",
#' "swe", "baseflow", "et", "rainfall",
#' "runoff", "snowfall", "soilMoist1", "Tair".
#' @param gcm Character. GCM name. One of "HadGEM2-ES", "CNRM-CM5", "CanESM2", "MIROC5", "ACCESS1-0",
#' "CCSM4", "CESM1-BGC", "CMCC-CMS", "GFDL-CM3", "HadGEM2-CC", "ens32avg",
#' "ens32max", "ens32min".
#' @param scenario Character. Emission scenario. One of "historical", "rcp45", "rcp85". Historical (default)
#' period covers 1950-2005. RCP45 and RCP85 scenarios cover 2006-2100.
#' @param period Character. Time aggregation. One of "day", "month", "year", "30yavg".
#' @param start_year Numeric. Start year for data retrieval.
#' @param end_year Numeric. End year for data retrieval.
#' @param out_dir Character. Output directory path to store downloaded data.
#' @param raster_path Character. (Optional) File path to a local raster file. If provided, the function
#' will load the raster from this path instead of using the Cal-Adapt API.
#'
#' @return A tibble row with appended climate raster in the `raster` column.
#' For points, the row will also include `lat` and `lon` columns.
#'
#' @examples
#' \dontrun{
#' # Polygon-level example
#' yolo_polygon <- ca_aoipreset_geom("counties") |>
#'   filter(state_name == "California") |>
#'   filter(name == "Yolo") |>
#'   select(state_name, county_name = name, geom) |>
#'   st_transform(4326)
#'
#' polygon_rasters <- download_caladapt_loca_raster(
#'   sf_obj = yolo_polygon,
#'   var = "pr",
#'   gcm = "CNRM-CM5",
#'   scenario = "rcp85",
#'   period = "year",
#'   start_year = 2006, end_year = 2100,
#'   out_dir = "data"
#' )
#' coords <- data.frame(lat = 36, lon = -120) |>
#'   st_as_sf(coords = c("lon", "lat"), crs = 4326)
#' point_rasters <- download_caladapt_loca_raster(
#'   sf_obj = coords,
#'   var = "pr",
#'   gcm = "CNRM-CM5",
#'   scenario = "rcp85",
#'   period = "year",
#'   start_year = 2006, end_year = 2100,
#'   out_dir = "data"
#' )
#' }
download_caladapt_loca_raster <- function(sf_obj,
                                          var = "pr",
                                          gcm = "HadGEM2-ES",
                                          scenario = "historical",
                                          period = "year",
                                          start_year, end_year,
                                          out_dir = "data",
                                          raster_path = NULL) {
  # First validate the spatial object.
  sf_obj <- .validate_sf_obj(sf_obj)
  .validate_caladapt_fn_inputs(var, gcm, scenario, period, start_year, end_year, sf_obj, raster_path, out_dir)

  geom_type <- unique(sf::st_geometry_type(sf_obj))
  
  # If polygon and large area, split into blocks.
  if (any(c("POLYGON", "MULTIPOLYGON") %in% geom_type)) {
    poly_area <- sf::st_area(sf_obj)
    if (poly_area > units::set_units(2000, "mi^2")) {
      polygon_blocks <- caladaptr::ca_biggeom_blocks(sf_obj, block_area_mi2 = 2000)
      if (nrow(polygon_blocks) > 1) {
        results <- purrr::map_dfr(polygon_blocks$geom, function(block_geom) {
          block <- sf::st_as_sf(tibble::tibble(geom = list(block_geom)),
                                 crs = sf::st_crs(sf_obj))
          request <- caladaptr::ca_apireq() |>
            caladaptr::ca_loc_sf(block) |>
            caladaptr::ca_gcm(gcm) |>
            caladaptr::ca_scenario(scenario) |>
            caladaptr::ca_period(period) |>
            caladaptr::ca_cvar(var) |>
            caladaptr::ca_years(start = start_year, end = end_year)
          y <- request |> caladaptr::ca_getrst_stars(out_dir = out_dir)
          block |> dplyr::mutate(
            var = var,
            gcm = gcm,
            scenario = scenario,
            period = period,
            start_year = start_year,
            end_year = end_year,
            raster = y
          )
        })
        return(results)
      }
    }
  }
  
  request <- caladaptr::ca_apireq() |>
    caladaptr::ca_loc_sf(sf_obj) |>
    caladaptr::ca_gcm(gcm) |>
    caladaptr::ca_scenario(scenario) |>
    caladaptr::ca_period(period) |>
    caladaptr::ca_cvar(var) |>
    caladaptr::ca_years(start = start_year, end = end_year)
  
  rast_file <- .try_get_raster(request, out_dir)
  if (is.null(rast_file)) { rast_file <- NA }
  
  res <- tibble::tibble(
    var = var,
    gcm = gcm,
    scenario = scenario,
    period = period,
    start_year = start_year,
    end_year = end_year,
    raster = rast_file
  )
  return(res)
}

## Helper functions (using PEcAn.logger instead of stop)

.validate_caladapt_fn_inputs <- function(var, gcm, scenario, period, start_year, end_year, sf_obj, raster_path, out_dir) {
  .validate_dates(scenario, start_year, end_year)
  .validate_gcm(gcm)
  .validate_period(period)
  .validate_var(var)
  .validate_raster_path(raster_path)
  .validate_out_dir(out_dir)
  invisible(TRUE)
}

.validate_dates <- function(scenario, start_year, end_year) {
  if (start_year > end_year) {
    PEcAn.logger::logger.error("Start year must be less than or equal to end year")
    return(invisible(NULL))
  }
  if (start_year < 1950 || end_year > 2100) {
    PEcAn.logger::logger.error("Start year must be >= 1950 and end year <= 2100")
    return(invisible(NULL))
  }
  if (scenario == "historical" && (start_year < 1950 || end_year > 2005)) {
    PEcAn.logger::logger.error("Historical period only covers 1950-2005")
    return(invisible(NULL))
  }
  if (scenario %in% c("rcp45", "rcp85") && (start_year < 2006 || end_year > 2100)) {
    PEcAn.logger::logger.error("RCP45 and RCP85 scenarios cover 2006-2100")
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_gcm <- function(gcm) {
  if (!exists("gcms", envir = environment())) {
    data(gcms, package = "caladaptr", envir = environment())
  }
  if (!gcm %in% gcms) {
    PEcAn.logger::logger.error("Invalid GCM name, must be one of: ", paste(gcms, collapse = ", "))
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_period <- function(period) {
  if (!exists("periods", envir = environment())) {
    data(periods, package = "caladaptr", envir = environment())
  }
  if (!period %in% periods) {
    PEcAn.logger::logger.error("Invalid period, must be one of: ", paste(periods, collapse = ", "))
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_var <- function(var) {
  if (!exists("cvars", envir = environment())) {
    data(cvars, package = "caladaptr", envir = environment())
  }
  if (!var %in% cvars) {
    PEcAn.logger::logger.error("Invalid variable, must be one of: ", paste(cvars, collapse = ", "))
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_raster_path <- function(raster_path) {
  if (!is.null(raster_path) && !is.character(raster_path)) {
    PEcAn.logger::logger.error("Raster path must be a character string")
    return(invisible(NULL))
  }
  if (!is.null(raster_path) && !file.exists(raster_path)) {
    PEcAn.logger::logger.error("The specified raster file does not exist: ", raster_path)
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_out_dir <- function(out_dir) {
  if (!is.character(out_dir)) {
    PEcAn.logger::logger.error("Output directory must be a character string")
    return(invisible(NULL))
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    PEcAn.logger::logger.info("Created new output directory: ", out_dir)
  }
  invisible(TRUE)
}

.validate_sf_obj <- function(sf_obj) {
  if (!inherits(sf_obj, c("sf", "sfc"))) {
    if (inherits(sf_obj, "SpatRaster")) {
      sf_obj <- terra::as_sf(sf_obj)
    } else {
      PEcAn.logger::logger.error("Points or polygon must be an 'sf', 'sfc' or 'SpatRaster' object")
      return(invisible(NULL))
    }
  }
  if (inherits(sf_obj, "sfc")) {
    sf_obj <- sf::st_sf(geometry = sf_obj)
  }
  if (!sf::st_is_valid(sf_obj)) {
    sf_obj <- sf::st_make_valid(sf_obj)
    if (!sf::st_is_valid(sf_obj)) {
      PEcAn.logger::logger.error("Polygon is not valid.")
      return(invisible(NULL))
    }
  }
  # California bounding box
  ca_bbox <- sf::st_polygon(list(
    matrix(c(-125.0, 32.0,
             -113.0, 32.0,
             -113.0, 44.0,
             -125.0, 44.0,
             -125.0, 32.0), ncol = 2, byrow = TRUE)
  )) |> sf::st_sfc(crs = sf::st_crs(sf_obj))
  geom_type <- unique(sf::st_geometry_type(sf_obj))
  if ("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) {
    intersection <- sf::st_intersection(sf_obj, ca_bbox)
    intersection_area <- if (nrow(sf::st_drop_geometry(intersection)) > 0) sf::st_area(intersection) else units::set_units(0, "m^2")
    if (intersection_area == units::set_units(0, "m^2")) {
      PEcAn.logger::logger.error("Polygon must be within California bounding box")
      return(invisible(NULL))
    } else if (intersection_area < 0.95 * sf::st_area(sf_obj)) {
      PEcAn.logger::logger.warn("Polygon is not fully within the Cal-Adapt bounding box. Intersection area: ", intersection_area, "; Polygon area: ", sf::st_area(sf_obj))
    }
  }
  if ("POINT" %in% geom_type) {
    within <- sf::st_within(sf_obj, ca_bbox, sparse = FALSE)
    if (!as.logical(all(within))) {
      PEcAn.logger::logger.error("Point must be within the Cal-Adapt bounding box")
      return(invisible(NULL))
    }
  }
  invisible(sf_obj)
}

.try_get_raster <- function(request, out_dir) {
  tryCatch(
    {
      res <- request |> caladaptr::ca_getrst_stars(out_dir = out_dir)
      if (is.null(res)) NA else res
    },
    error = function(e) {
      if (grepl("Gateway Timeout", e$message, ignore.case = TRUE)) {
        PEcAn.logger::logger.warn("Gateway timeout encountered; returning NA for raster.")
        NA
      } else {
        PEcAn.logger::logger.error(e)
      }
    }
  )
}