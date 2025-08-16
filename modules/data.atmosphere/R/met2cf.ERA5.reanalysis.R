#' met2CF.ERA5.reanalysis - function for ERA5 reanalysis data processing
#'
#' @param slat latitude
#' @param slon longitude
#' @param in.path path to the directory containing the ERA5 reanalysis NetCDF files
#' @param start_date start date
#' @param end_date end date
#' @param sitename the name of the site used for making the identifier
#' @param outfolder path to directory where CF-compliant nc files need to be saved
#' @param in.prefix initial portion of the filename that does not vary by date 
#' @param vars variables to be extracted. If NULL all available variables will be returned
#' @param overwrite logical if files need to be overwritten
#' @param verbose logical flag defining if output of function be extra verbose
#'
#' @return dataframe with file information for processed NetCDF files
#' @export
#' @author Akash
met2CF.ERA5.reanalysis <- function(slat, slon, in.path, start_date, end_date, sitename, outfolder,
                                   in.prefix, vars = NULL, overwrite = FALSE, verbose = TRUE) {

  if (is.na(slat) || is.na(slon)) {
    PEcAn.logger::logger.severe("Invalid latitude or longitude provided")
    return(NULL)
  }
  years <- seq(lubridate::year(start_date), lubridate::year(end_date), 1)

  # Build mapping from native ERA5 names to CF names and CF units using the standard table if available
  cf_units <- NULL
  era5_to_cf <- NULL
  if (exists("pecan_standard_met_table", inherits = TRUE)) {
    tbl <- pecan_standard_met_table
    # try common column names; ignore if not present
    has_cf <- "cf_standard_name" %in% names(tbl)
    has_units <- "units" %in% names(tbl)
    has_era5 <- "era5" %in% names(tbl)
    if (has_cf && has_units && has_era5) {
      tbl <- tbl[!is.na(tbl$era5) & nzchar(tbl$era5), , drop = FALSE]
      era5_to_cf <- stats::setNames(tbl$cf_standard_name, tbl$era5)
      cf_units <- stats::setNames(gsub("/", " ", tbl$units), tbl$cf_standard_name)
    }
  }

  # Fallback minimal mapping if table not available
  if (is.null(era5_to_cf)) {
    era5_to_cf <- c(t2m = "air_temperature", d2m = "dew_point_temperature", sp = "air_pressure",
                    ssrd = "surface_downwelling_shortwave_flux_in_air",
                    strd = "surface_downwelling_longwave_flux_in_air",
                    tp = "precipitation_flux",
                    u10 = "eastward_wind", v10 = "northward_wind")
  }

  tryCatch({
    # process each year file and row-bind
    out.xts <- years %>%
      purrr::map(function(year) {
        ncfile <- file.path(in.path, paste0(in.prefix, year, ".nc"))
        if (!file.exists(ncfile)) {
          PEcAn.logger::logger.warn(paste0("File not found: ", ncfile))
          return(NULL)
        }
        nc_data <- ncdf4::nc_open(ncfile)
        on.exit(ncdf4::nc_close(nc_data), add = TRUE)

        # time
        time_var <- intersect(c("time", "valid_time"), names(nc_data$var))
        if (length(time_var) == 0) {
          if ("time" %in% names(nc_data$dim)) {
            t_vals <- nc_data$dim$time$vals
            t_units <- nc_data$dim$time$units
          } else {
            PEcAn.logger::logger.warn(paste0("No time variable in ", ncfile))
            return(NULL)
          }
        } else {
          t_vals <- ncdf4::ncvar_get(nc_data, time_var[1], collapse_degen = FALSE)
          t_units <- ncdf4::ncatt_get(nc_data, time_var[1], "units")$value
        }
        timestamp <- PEcAn.utils::cf2datetime(t_vals, t_units, tz = "UTC")

        # grid
        lon <- ncdf4::ncvar_get(nc_data, "longitude")
        lat <- ncdf4::ncvar_get(nc_data, "latitude")
        lon_ix <- which.min(abs(lon - slon))
        lat_iy <- which.min(abs(lat - slat))

        # select variables to extract (accept either native ERA5 names or CF names in `vars`)
        if (is.null(vars)) {
          requested_native <- names(nc_data$var)
        } else {
          # map CF -> native where applicable
          cf_to_native <- stats::setNames(names(era5_to_cf), unname(era5_to_cf))
          requested_native <- unique(c(
            # native names directly present
            vars[vars %in% names(nc_data$var)],
            # CF names converted back to native
            cf_to_native[vars[vars %in% names(cf_to_native)]]
          ))
        }
        # keep only present and likely 3D phys vars
        requested_native <- intersect(requested_native, names(nc_data$var))
        requested_native <- setdiff(requested_native, c("longitude", "latitude", "time", "valid_time"))
        if (!length(requested_native)) return(NULL)

        extracted <- requested_native %>%
          purrr::set_names(requested_native) %>%
          purrr::map_dfc(function(vname) {
            vals <- ncdf4::ncvar_get(nc_data, vname, collapse_degen = FALSE)
            # Accept (lon,lat,time), (1,1,time), or vector(time)
            if (length(dim(vals)) == 3) {
              point_vals <- vals[lon_ix, lat_iy, ]
            } else if (is.null(dim(vals))) {
              point_vals <- vals
            } else if (identical(dim(vals), c(1, 1, length(timestamp)))) {
              point_vals <- vals[1, 1, ]
            } else {
              PEcAn.logger::logger.warn(paste0("Unexpected dimensions for variable ", vname))
              point_vals <- rep(NA_real_, length(timestamp))
            }
            as.numeric(point_vals)
          })
        xts::xts(extracted, order.by = timestamp)
      }) %>% purrr::discard(is.null)

    if (!length(out.xts)) {
      PEcAn.logger::logger.severe("No data successfully extracted from any files")
      return(NULL)
    }

    out.new <- do.call(xts::rbind.xts, out.xts)
    native_vars_present <- colnames(out.new)

    # Convert accumulations/rates
    time_diffs <- diff(as.numeric(zoo::index(out.new)))
    timestep_seconds <- if (length(time_diffs)) as.numeric(median(time_diffs)) else 3600
    if ("ssrd" %in% native_vars_present) out.new[, "ssrd"] <- out.new[, "ssrd"] / timestep_seconds
    if ("strd" %in% native_vars_present) out.new[, "strd"] <- out.new[, "strd"] / timestep_seconds
    if ("tp" %in% native_vars_present) out.new[, "tp"] <- (out.new[, "tp"] * 1000) / timestep_seconds

    # Specific humidity via vectorized RH if possible
    if (all(c("t2m", "d2m", "sp") %in% native_vars_present)) {
      T_k  <- as.numeric(out.new[, "t2m"])   # K
      Td_k <- as.numeric(out.new[, "d2m"])   # K
      T_c  <- T_k  - 273.15
      Td_c <- Td_k - 273.15
      es <- 6.112 * exp((17.62 * T_c)  / (243.12 + T_c))
      e  <- 6.112 * exp((17.62 * Td_c) / (243.12 + Td_c))
      rh_prop <- pmin(pmax(e / es, 0), 1)
      q <- PEcAn.data.atmosphere::rh2qair(rh_prop, T_k, as.numeric(out.new[, "sp"]))
      out.new <- xts::merge.xts(out.new, xts::xts(q, order.by = zoo::index(out.new)))
      colnames(out.new)[ncol(out.new)] <- "specific_humidity"
    }

    # Map to CF names
    mappable_native <- intersect(names(era5_to_cf), native_vars_present)
    cf_data <- out.new[, mappable_native, drop = FALSE]
    colnames(cf_data) <- era5_to_cf[mappable_native]
    # Preserve derived specific_humidity if present (not in era5_to_cf mapping)
    if ("specific_humidity" %in% colnames(out.new)) {
      cf_data <- xts::merge.xts(cf_data, out.new[, "specific_humidity", drop = FALSE])
    }

    # Units vector, same length as variables
    cf_var_names <- colnames(cf_data)
    cf_var_units <- purrr::map_chr(cf_var_names, function(nm) {
      u <- if (!is.null(cf_units)) unname(cf_units[nm]) else NA_character_
      if (length(u) == 0 || is.na(u)) {
        if (identical(nm, "specific_humidity")) return("1")
        return(NA_character_)
      }
      as.character(u)
    })
    names(cf_var_units) <- cf_var_names

    # Prepare output folder and file per year
    identifier <- paste("ERA5", sitename, "reanalysis", sep = "_")
    out_dir <- file.path(outfolder, identifier)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    start_date_out <- min(zoo::index(cf_data))
    end_date_out <- max(zoo::index(cf_data))
    results <- data.frame(
      file = "", host = PEcAn.remote::fqdn(), mimetype = "application/x-netcdf",
      formatname = "CF Meteorology", startdate = format(start_date_out, "%Y-%m-%dT%H:%M:00 %z"),
      enddate = format(end_date_out, "%Y-%m-%dT%H:%M:%S %z"), dbfile.name = "ERA5.reanalysis",
      stringsAsFactors = FALSE
    )

    years %>% purrr::walk(function(year) {
      year_data <- cf_data[format(zoo::index(cf_data), "%Y") == as.character(year)]
      if (!nrow(year_data)) return(NULL)
      flname <- file.path(out_dir, paste(paste("ERA5", "reanalysis", year, sep = "."), "nc", sep = "."))
      if (!file.exists(flname) || overwrite) {
        time_vals <- as.numeric(zoo::index(year_data))
        time_dim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01 00:00:00", time_vals, create_dimvar = TRUE)
        lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", slat, create_dimvar = TRUE)
        lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", slon, create_dimvar = TRUE)
        nc_vars <- purrr::map2(cf_var_names, cf_var_units, ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), missval = NA_real_))
        nc <- ncdf4::nc_create(flname, nc_vars, verbose = FALSE)
        for (j in seq_along(cf_var_names)) {
          ncdf4::ncvar_put(nc, nc_vars[[j]], zoo::coredata(year_data)[, j])
        }
        ncdf4::nc_close(nc)
      }
    })

    first_year <- years[1]
    results$file <- file.path(out_dir, paste(paste("ERA5", "reanalysis", first_year, sep = "."), "nc", sep = "."))
    return(results)

  }, error = function(e) {
    PEcAn.logger::logger.severe(paste0("Error in met2CF.ERA5.reanalysis: ", conditionMessage(e)))
    return(NULL)
  })
}
