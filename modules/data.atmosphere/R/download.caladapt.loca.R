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
#' @param outfolder Character. Output directory path to store downloaded data. (Required, no default)
#' @param start_date Numeric. Start date (year) for data retrieval. Accepts YYYY-MM-DD format, but caladaptr functions only use the year.
#' @param end_date Numeric. End date (year) for data retrieval. Accepts YYYY-MM-DD format, but caladaptr functions only use the year.
#' @param sf_obj An `sf`, `sfc`, or `SpatVector` object representing the points or polygon boundary.
#' @param var Character. Climate variable to retrieve. One of "tasmax", "tasmin", "pr",
#' "swe", "baseflow", "et", "rainfall",
#' "runoff", "snowfall", "soilMoist1", "Tair".
#' @param gcm Character. GCM name. One of "HadGEM2-ES", "CNRM-CM5", "CanESM2", "MIROC5", "ACCESS1-0",
#' "CCSM4", "CESM1-BGC", "CMCC-CMS", "GFDL-CM3", "HadGEM2-CC", "ens32avg",
#' "ens32max", "ens32min".
#' @param scenario Character. Emission scenario. One of "historical", "rcp45", "rcp85".
#' @param period Character. Time aggregation. One of "day", "month", "year", "30yavg".
#'
#' @return A tibble row with appended climate raster in the `raster` column.
#' For points, the row will also include `lat` and `lon` columns.
#'
#' @examples
#' \dontrun{
#'  library(caladaptr)
#'  yolo_polygon <- caladaptr::ca_aoipreset_geom("counties") |>
#'    dplyr::filter(state_name == "California") |>
#'    dplyr::filter(name == "Yolo") |>
#'    dplyr::select(state_name, county_name = name, geom) |>
#'    sf::st_transform(4326)

#'  polygon_rasters <- download.caladapt.loca(
#'    outfolder = tempdir(),
#'    start_date = 2006, end_date = 2100,
#'    sf_obj = yolo_polygon,
#'    var = "pr",
#'    gcm = "CNRM-CM5",
#'    scenario = "rcp85",
#'    period = "year"
#'  )
#'  z <- stars::read_stars(polygon_rasters$raster)
#'  plot(z)
#'  # There is also a caladaptr function ca_stars_read
#'  # that returns a list of stars objects.

#'  coords <- data.frame(lat = 36, lon = -120) |>
#'    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
#'  point_rasters <- download.caladapt.loca(
#'    outfolder = tempdir(),
#'    start_date = 2006, end_date = 2100,
#'    sf_obj = coords,
#'    var = "pr",
#'    gcm = "CNRM-CM5",
#'    scenario = "rcp85",
#'    period = "year"
#'  )
#' }
#' @export
download.caladapt.loca <- function(outfolder, start_date, end_date, sf_obj, 
                                   var, gcm, scenario, period) {
  
  # following is required so that caladaptr functions can be used (seems to be a bug with caladaptr functions)
  data(list = c("ca_baseurl", "gcms", "cvars", "periods", "scenarios"), package = "caladaptr")
  # handled separately b/c fixes object as side effect
  sf_obj <- .validate_sf_obj(sf_obj)
  start_year <- .validate_years(start_date)
  end_year <- .validate_years(end_date)

  # Validate inputs using extracted years
  .validate_caladapt_fn_inputs(
    outfolder, start_year, end_year, 
    var, gcm, scenario, period
  )

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
          y <- request |> caladaptr::ca_getrst_stars(out_dir = outfolder)
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
  
  rast_file <- .try_get_raster(request, outfolder)
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

.validate_caladapt_fn_inputs <- function(outfolder, start_date, end_date, 
                                          var, gcm, scenario, period) {
  .validate_out_dir(outfolder)
  .validate_dates(scenario, start_date, end_date)
  .validate_var(var)
  .validate_gcm(gcm)
  .validate_scenario(scenario)
  .validate_period(period)
  invisible(TRUE)
}

.validate_dates <- function(scenario, start_date, end_date) {
  if (is.character(start_date)) {
    start_date <- as.numeric(substr(start_date, 1, 4))
  }
  if (start_date > end_date) {
    PEcAn.logger::logger.error("Start date must be less than or equal to end date")
    return(invisible(NULL))
  }
  if (start_date < 1950 || end_date > 2100) {
    PEcAn.logger::logger.error("Start date must be >= 1950 and end date <= 2100")
    return(invisible(NULL))
  }
  if (scenario == "historical" && (start_date < 1950 || end_date > 2005)) {
    PEcAn.logger::logger.error("Historical period only covers 1950-2005")
    return(invisible(NULL))
  }
  if (scenario %in% c("rcp45", "rcp85") && (start_date < 2006 || end_date > 2100)) {
    PEcAn.logger::logger.error("RCP45 and RCP85 scenarios cover 2006-2100")
    return(invisible(NULL))
  }
  invisible(TRUE)
}



.validate_gcm <- function(gcm) {
  if (!gcm %in% caladaptr::gcms) {
    PEcAn.logger::logger.error(
      "Invalid GCM name, must be one of: ",
      paste(caladaptr::gcms, collapse = ", ")
    )
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_scenario <- function(scenario) {
  if (!scenario %in% caladaptr::scenarios) {
    PEcAn.logger::logger.error(
      "Invalid scenario, must be one of: ",
      paste(caladaptr::scenarios, collapse = ", ")
    )
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_period <- function(period) {
  valid_periods <- c("day", "month", "year", "30yavg")
  if (!period %in% caladaptr::periods) {
    PEcAn.logger::logger.error(
      "Invalid period, must be one of: ",
      paste(caladaptr::periods, collapse = ", ")
    )
    return(invisible(NULL))
  }
  invisible(TRUE)
}

.validate_out_dir <- function(outfolder) {
  if (!is.character(outfolder)) {
    PEcAn.logger::logger.error("Output folder must be a character string")
    return(invisible(NULL))
  }
  if (!dir.exists(outfolder)) {
    dir.create(outfolder, recursive = TRUE)
    PEcAn.logger::logger.info("Created new output directory: ", outfolder)
  }
  invisible(TRUE)
}

.validate_sf_obj <- function(sf_obj) {
  if (!inherits(sf_obj, c("sf", "sfc"))) {
    if (inherits(sf_obj, "SpatVector")) {
      sf_obj <- sf::st_as_sf(sf_obj)
    } else {
      PEcAn.logger::logger.error("Points or polygon must be an 'sf', 'sfc' or 'SpatVector' object")
      stop("Points or polygon must be an 'sf', 'sfc' or 'SpatVector' object")
    }
  }
  if (inherits(sf_obj, "sfc")) {
    sf_obj <- sf::st_sf(geometry = sf_obj)
  }
  if (!sf::st_is_valid(sf_obj)) {
    sf_obj <- sf::st_make_valid(sf_obj)
    if (!sf::st_is_valid(sf_obj)) {
      PEcAn.logger::logger.error("Polygon is not valid.")
      stop("Polygon is not valid.")
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
      stop("Polygon must be within California bounding box")
    } else if (intersection_area < 0.95 * sf::st_area(sf_obj)) {
      PEcAn.logger::logger.warn("Polygon is not fully within the Cal-Adapt bounding box. Intersection area: ", intersection_area, "; Polygon area: ", sf::st_area(sf_obj))
    }
  }
  if ("POINT" %in% geom_type) {
    within <- sf::st_within(sf_obj, ca_bbox, sparse = FALSE)
    if (!all(within)) {
      PEcAn.logger::logger.error("Point must be within the Cal-Adapt bounding box")
      stop("Point must be within the Cal-Adapt bounding box")
    }
  }
  return(sf_obj)
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

.validate_years <- function(date){
  if (is.numeric(date) && date >= 1950 && date <= 2100) {
    return(as.integer(date))
  }
  if (is.character(date)) {
    date <- lubridate::year(lubridate::ymd(date))
    if (is.na(date)) {
      PEcAn.logger::logger.error("Invalid date. Must be in YYYY or YYYY-MM-DD format.")
      return(NA)
    }
  }
  if(date < 1950 || date > 2100) {
    PEcAn.logger::logger.error("Date must be between 1950 and 2100")
    return(NA)
  }
  return(date)
}

.validate_var <- function(var) {
  if (!var %in% caladaptr::cvars) {
    PEcAn.logger::logger.error(
      "Invalid variable, must be one of: ",
      paste(caladaptr::cvars, collapse = ", ")
    )
    return(invisible(NULL))
  }
  invisible(TRUE)
}
