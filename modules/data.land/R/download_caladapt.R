# Retrieve Cal-Adapt climate raster for a polygon or a point

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
#' - [Cal Adapt Data Catalog](https://berkeley-gif.github.io/caladapt-docs/data-catalog.html):
#'
#' @param polygon An `sf` or `sfc` geometry row representing the polygon boundary
#'   (required for `ca_fetch_raster_polygon()`).
#' @param lat Numeric. Latitude of the point (required for `ca_fetch_raster_point()`).
#' @param lon Numeric. Longitude of the point (required for `ca_fetch_raster_point()`).
#' @param var Character. Climate variable to retrieve. One of "tasmax", "tasmin", "pr",
#' "swe", "baseflow", "et", "rainfall",
#' "runoff", "snowfall", "soilMoist1", "Tair"
#' @param gcm Character. GCM name. One of "HadGEM2-ES", "CNRM-CM5", "CanESM2", "MIROC5", "ACCESS1-0",
#' "CCSM4", "CESM1-BGC", "CMCC-CMS", "GFDL-CM3", "HadGEM2-CC", "ens32avg",
#' "ens32max", "ens32min".
#' @param scenario Character. Emission scenario. One of "historical", "rcp45", "rcp85".
#' @param period Character. Time aggregation. One of "day", "month", "year", "30yavg"
#' @param start_year Numeric. Start year for data retrieval.
#' @param end_year Numeric. End year for data retrieval.
#' @param out_dir Character. Output directory path to store downloaded data.
#' @param raster_path Character. (Optional) File path to a local raster file. If provided, the function will load the raster from this path instead of using the Cal-Adapt API.
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
#' polygon_data <- ca_fetch_raster_polygon(
#'   polygon = yolo_polygon,
#'   var = "pr",
#'   gcm = "CNRM-CM5",
#'   scenario = "rcp85",
#'   period = "year",
#'   start_year = 2006, end_year = 2100,
#'   out_dir = "data"
#' )
#'
#' # Point-level example
#' point_data <- ca_fetch_raster_point(
#'   lat = 36.7783, lon = -119.4179,
#'   var = "tasmax",
#'   gcm = "HadGEM2-ES",
#'   scenario = "rcp45",
#'   period = "year",
#'   start_year = 2006, end_year = 2100,
#'   out_dir = "data"
#' )
#' }
NULL
# Function for polygons
ca_fetch_raster_polygon <- function(polygon,
                                    var = "pr",
                                    gcm = "HadGEM2-ES",
                                    scenario = "historical",
                                    period = "year",
                                    start_year, end_year,
                                    out_dir = "data") {
  # Check that polygon is valid
  if (!sf::st_is_valid(polygon)) {
    PEcAn.logger::logger.warn("Polygon is not valid, attempting to fix")
    polygon <- sf::st_make_valid(polygon)
    if (!sf::st_is_valid(polygon)) {
      PEcAn.logger::logger.error("Polygon is still not valid, aborting")
    }
  }


  # Check that polygon is within (a buffered) Cal Adapt Domain
  check_bbox_polygon <- st_polygon(
    list(
      matrix(
        c(
          -125.0, 32.0,
          -113.0, 32.0,
          -113.0, 44.0,
          -125.0, 44.0,
          -125.0, 32.0
        ),
        ncol = 2, byrow = TRUE
      )
    )
  ) |>
    st_sfc(crs = st_crs(polygon))
  polygon_inside_caladapt <- st_contains(check_bbox_polygon, polygon, sparse = FALSE)

  if (!polygon_inside_caladapt) {
    PEcAn.logger::logger.error("Polygon must be within the bounding box xmin: -124.7, ymin: 32.5, xmax: -113.5, ymax: 43.7")
  }

  # If polygon is too large, split it into smaller blocks
  if (st_area(polygon) > units::set_units(18000, "mi^2")) {
    PEcAn.logger::logger.warn(
      "Polygon is too large for Cal-Adapt API",
      "splitting into smaller blocks..."
    )
    polygon <- caladaptr::ca_biggeom_blocks(polygon, block_area_mi2 = 18000)
  }

  # Check that out_dir exists, if not, create
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  # Build caladapt request
  request <- caladaptr::ca_apireq() |>
    caladaptr::ca_loc_sf(polygon) |>
    caladaptr::ca_gcm(gcm) |>
    caladaptr::ca_scenario(scenario) |>
    caladaptr::ca_period(period) |>
    caladaptr::ca_cvar(var) |>
    caladaptr::ca_years(start = start_year, end = end_year)

  y <- request |>
    caladaptr::ca_getrst_stars(out_dir = out_dir, overwrite = TRUE, sidecar = TRUE, debug = TRUE)

  ret <- polygon |>
    dplyr::mutate(
      var = var,
      gcm = gcm,
      scenario = scenario,
      period = period,
      start_year = start_year,
      end_year = end_year,
      raster = y
    )
  return(ret)
}

# Function for points
ca_fetch_raster_point <- function(lat, lon,
                                  var = "pr",
                                  gcm = "HadGEM2-ES",
                                  scenario = "historical",
                                  period = "year",
                                  start_year, end_year,
                                  out_dir = "data",
                                  raster_path = NULL) {
  # Check that point is numeric and within Cal Adapt bounding box
  if (!is.numeric(lat) || !is.numeric(lon)) {
    stop("Latitude and longitude must be numeric")
  }
  if (lon < -124.7 || lon > -113.5 || lat < 32.5 || lat > 43.7) {
    stop("Point must be within the bounding box xmin: -124.7, ymin: 32.5, xmax: -113.5, ymax: 43.7")
  }

  if (!is.null(raster_path)) {
    if (!file.exists(raster_path)) {
      stop(paste("The specified raster file does not exist:", raster_path))
    }

    res_rast <- stars::read_stars(raster_path)
  } else { # If no local raster path is provided, fetch the raster via the API
    request <- caladaptr::ca_apireq() |>
      caladaptr::ca_loc_pt(lat, lon) |>
      caladaptr::ca_gcm(gcm) |>
      caladaptr::ca_scenario(scenario) |>
      caladaptr::ca_period(period) |>
      caladaptr::ca_cvar(var) |>
      caladaptr::ca_years(start = start_year, end = end_year)

    res_rast <- request |>
      caladaptr::ca_getrst_stars(out_dir = out_dir)
  }

  # Construct the return tibble
  res <- tibble::tibble(
    lat         = lat,
    lon         = lon,
    var         = var,
    gcm         = gcm,
    scenario    = scenario,
    period      = period,
    start_year  = start_year,
    end_year    = end_year,
    raster      = res_rast
  )

  return(res)
}
