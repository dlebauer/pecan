#' Merge multiple NetCDF files into one
#'
#' @param files \code{character}. List of filepaths, which should lead to NetCDF files.
#' @param outfile \code{character}. Output filename of the merged data.
#' @return A NetCDF file containing all of the merged data.
#' @examples
#' \dontrun{
#' files <- list.files(paste0(system.file(package="processNC"), "/extdata"),
#'                     pattern="tas.*\\.nc", full.names=TRUE)
#' temp <- tempfile(fileext=".nc")
#' mergeNC(files=files, outfile=temp)
#' terra::rast(temp)
#' }
#' @export mergeNC
#' @name mergeNC
#' @source https://github.com/RS-eco/processNC/blob/main/R/mergeNC.R
mergeNC <- function(
    ##title<< Aggregate data in netCDF files
  files ##<< character vector: names of the files to merge
  , outfile ##<< character: path to save the results files to.
)
  ##description<<
  ## This function aggregates time periods in netCDF files. Basically it is just a
  ## wrapper around the respective cdo function.
{
  ## supply cdo command
  cdoCmd <- paste("cdo -cat", paste(files, collapse = " "), outfile, sep = " ")

  ## run command
  system(cdoCmd)
  cat(paste("Created file ", outfile, ".\n", sep = ""))

  ## character string: name of the file created.
  invisible(outfile)
}

.sipnet_read_output_table <- function(path, required_cols, optional_cols) {
  read_header <- function(skip_lines) {
    try(
      data.table::fread(
        path,
        skip = skip_lines,
        nrows = 0,
        data.table = FALSE,
        check.names = FALSE,
        showProgress = FALSE
      ),
      silent = TRUE
    )
  }

  header <- read_header(0L)
  skip_used <- 0L
  if (inherits(header, "try-error") || !("year" %in% names(header))) {
    header <- read_header(1L)
    skip_used <- 1L
  }

  if (inherits(header, "try-error") || !("year" %in% names(header))) {
    PEcAn.logger::logger.error("Unable to parse SIPNET output file header", path)
  }

  available_cols <- names(header)
  missing_required <- setdiff(required_cols, available_cols)
  if (length(missing_required) > 0) {
    PEcAn.logger::logger.error(
      "Missing required SIPNET output columns:",
      paste(missing_required, collapse = ", ")
    )
  }

  read_cols <- c(required_cols, intersect(optional_cols, available_cols))
  int_cols <- intersect(c("year", "day"), read_cols)
  num_cols <- setdiff(read_cols, int_cols)

  sipnet_output <- try(
    data.table::fread(
      path,
      skip = skip_used,
      select = read_cols,
      colClasses = list(integer = int_cols, numeric = num_cols),
      data.table = FALSE,
      check.names = FALSE,
      showProgress = FALSE
    ),
    silent = TRUE
  )

  if (inherits(sipnet_output, "try-error")) {
    PEcAn.logger::logger.error("Unable to parse SIPNET output file", path)
  }

  sipnet_output
}

#--------------------------------------------------------------------------------------------------#
##' Convert SIPNET output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##'
##' @param outdir Location of SIPNET model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param revision model revision
##' @param overwrite Flag for overwriting nc files or not
##' @param conflict Flag for dealing with conflicted nc files, if T we then will merge those, if F we will jump to the next.
##' @param prefix prefix to read the output files
##' @param delete.raw logical: remove sipnet.out files after converting?
##'
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.SIPNET <- function(outdir, sitelat, sitelon, start_date, end_date, delete.raw = FALSE, revision, prefix = "sipnet.out",
                                overwrite = FALSE, conflict = FALSE) {
  profile_enabled <- tolower(trimws(Sys.getenv("PECAN_SIPNET_PROFILE", ""))) %in% c("1", "true", "yes", "on")
  timings <- list()
  tic <- function() proc.time()[["elapsed"]]
  toc <- function(stage, started_at) {
    if (!profile_enabled) {
      return(invisible(NULL))
    }
    timings[[length(timings) + 1]] <<- data.frame(
      function_name = "model2netcdf.SIPNET",
      stage = stage,
      elapsed_seconds = unname(proc.time()[["elapsed"]] - started_at),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  write_profile_csv <- trimws(Sys.getenv("PECAN_SIPNET_PROFILE_CSV", ""))
  write_profile_csv <- if (profile_enabled) {
    if (tolower(write_profile_csv) %in% c("1", "true", "yes", "on")) {
      file.path(outdir, "sipnet_model2netcdf.profile.csv")
    } else if (nzchar(write_profile_csv)) {
      write_profile_csv
    } else {
      ""
    }
  } else {
    ""
  }

  flush_timings <- function() {
    if (!profile_enabled || length(timings) == 0) {
      return(invisible(NULL))
    }
    timing_df <- do.call(rbind, timings)
    summary_txt <- apply(timing_df, 1, function(x) {
      paste0(x[["function_name"]], " ", x[["stage"]], ": ", signif(as.numeric(x[["elapsed_seconds"]]), 4), " s")
    })
    PEcAn.logger::logger.info("SIPNET profiling summary:\n", paste(summary_txt, collapse = "\n"), wrap = FALSE)
    if (nzchar(write_profile_csv)) {
      utils::write.table(
        timing_df,
        file = write_profile_csv,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists(write_profile_csv),
        quote = TRUE,
        append = file.exists(write_profile_csv)
      )
    }
    invisible(NULL)
  }
  on.exit(flush_timings(), add = TRUE)

  t_total <- tic()
  sipnet_out_file <- file.path(outdir, prefix)

  required_cols <- c(
    "year", "day", "time", "gpp", "rAboveground", "rRoot", "rtot", "rSoil", "nee",
    "plantWoodC", "plantLeafC", "coarseRootC", "fineRootC", "soil", "litter",
    "fluxestranspiration", "soilWater", "soilWetnessFrac", "snow"
  )
  optional_cols <- c("litterWater", "evapotranspiration", "npp", "woodCreation")

  t_read <- tic()
  sipnet_output <- .sipnet_read_output_table(sipnet_out_file, required_cols, optional_cols)
  toc("read_sipnet_output", t_read)

  t_prepare <- tic()
  simulation_years <- sort(unique(sipnet_output$year))
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))

  if (!all(year_seq %in% simulation_years)) {
    PEcAn.logger::logger.severe("Years selected for model run and SIPNET output years do not match")
  }

  year_index <- split(seq_len(nrow(sipnet_output)), sipnet_output$year)

  first_year <- simulation_years[1]
  first_day <- unique(sipnet_output$day[sipnet_output$year == first_year])[1]
  out_day <- sum(sipnet_output$year == first_year & sipnet_output$day == first_day, na.rm = TRUE)
  if (!is.finite(out_day) || out_day <= 0) {
    PEcAn.logger::logger.error("Unable to infer SIPNET output timestep from parsed output")
  }

  timestep.s <- 86400 / out_day

  run_dir <- if (grepl("/out/", outdir, fixed = TRUE)) {
    sub("/out/", "/run/", outdir, fixed = TRUE)
  } else {
    sub("/out/?$", "/run/", outdir)
  }
  if (identical(run_dir, outdir)) {
    PEcAn.logger::logger.error("Cannot infer run directory from outdir; expected '/out/' in path", outdir)
  }

  run_param_file <- file.path(run_dir, "sipnet.param")
  if (!file.exists(run_param_file)) {
    PEcAn.logger::logger.error("Missing SIPNET parameter file", run_param_file)
  }

  param <- utils::read.table(run_param_file, stringsAsFactors = FALSE)
  id <- which(param[, 1] == "leafCSpWt")
  SLA <- 1000 / param[id, 2]

  worker_env <- trimws(Sys.getenv("PECAN_SIPNET_NC_WORKERS", ""))
  if (nzchar(worker_env)) {
    worker_setting <- suppressWarnings(as.integer(worker_env))
    if (is.na(worker_setting) || worker_setting < 1L) {
      PEcAn.logger::logger.error("PECAN_SIPNET_NC_WORKERS must be an integer >= 1")
    }
  } else {
    detected <- suppressWarnings(as.integer(parallel::detectCores()))
    if (is.na(detected) || detected < 1L) {
      detected <- 1L
    }
    worker_setting <- max(1L, detected - 1L)
  }

  if (conflict && !nzchar(Sys.which("cdo"))) {
    PEcAn.logger::logger.error("'conflict=TRUE' requires cdo in PATH")
  }

  if (!("litterWater" %in% names(sipnet_output))) {
    sipnet_output$litterWater <- NA_real_
  }
  if (!("woodCreation" %in% names(sipnet_output))) {
    sipnet_output$woodCreation <- NA_real_
  }

  if (revision == "unk") {
    if (!("npp" %in% names(sipnet_output))) {
      PEcAn.logger::logger.error("SIPNET output missing 'npp' required for revision='unk'")
    }
    qle_source <- sipnet_output$npp
  } else if ("evapotranspiration" %in% names(sipnet_output)) {
    qle_source <- sipnet_output$evapotranspiration
  } else if ("npp" %in% names(sipnet_output)) {
    qle_source <- sipnet_output$npp
  } else {
    PEcAn.logger::logger.error("SIPNET output missing both 'evapotranspiration' and 'npp' required for Qle")
  }

  timestamps_utc <- as.POSIXct(
    strptime(sprintf("%04d %03d", sipnet_output$year, sipnet_output$day), "%Y %j", tz = "UTC")
  ) + sipnet_output$time * 3600

  year_origin <- as.POSIXct(
    paste0(year_seq, "-01-01 00:00:00"),
    tz = "UTC"
  )
  names(year_origin) <- as.character(year_seq)

  output_all <- list(
    "GPP" = (sipnet_output$gpp * 0.001) / timestep.s,
    "NPP" = (sipnet_output$gpp * 0.001) / timestep.s -
      ((sipnet_output$rAboveground * 0.001) / timestep.s + (sipnet_output$rRoot * 0.001) / timestep.s),
    "TotalResp" = (sipnet_output$rtot * 0.001) / timestep.s,
    "AutoResp" = (sipnet_output$rAboveground * 0.001) / timestep.s + (sipnet_output$rRoot * 0.001) / timestep.s,
    "HeteroResp" = ((sipnet_output$rSoil - sipnet_output$rRoot) * 0.001) / timestep.s,
    "SoilResp" = (sipnet_output$rSoil * 0.001) / timestep.s,
    "NEE" = (sipnet_output$nee * 0.001) / timestep.s,
    "AbvGrndWood" = (sipnet_output$plantWoodC * 0.001),
    "leaf_carbon_content" = (sipnet_output$plantLeafC * 0.001),
    "TotLivBiom" = (sipnet_output$plantWoodC * 0.001) + (sipnet_output$plantLeafC * 0.001) +
      (sipnet_output$coarseRootC + sipnet_output$fineRootC) * 0.001,
    "TotSoilCarb" = (sipnet_output$soil * 0.001) + (sipnet_output$litter * 0.001),
    "Qle" = (qle_source * 10 * PEcAn.data.atmosphere::get.lv()) / timestep.s,
    "Transp" = (sipnet_output$fluxestranspiration * 10) / timestep.s,
    "SoilMoist" = (sipnet_output$soilWater * 10),
    "SoilMoistFrac" = sipnet_output$soilWetnessFrac,
    "SWE" = (sipnet_output$snow * 10),
    "litter_carbon_content" = sipnet_output$litter * 0.001,
    "litter_mass_content_of_water" = sipnet_output$litterWater * 10,
    "LAI" = (sipnet_output$plantLeafC * 0.001) * SLA,
    "fine_root_carbon_content" = sipnet_output$fineRootC * 0.001,
    "coarse_root_carbon_content" = sipnet_output$coarseRootC * 0.001,
    "GWBI" = (sipnet_output$woodCreation * 0.001) / 86400,
    "AGB" = (sipnet_output$plantWoodC + sipnet_output$plantLeafC) * 0.001
  )
  toc("prepare_conversion_inputs", t_prepare)

  write_year_file <- function(y, row_ids, target_file) {
    if (length(row_ids) == 0) {
      return(invisible(NULL))
    }

    sub_dates_cf <- as.numeric(
      difftime(timestamps_utc[row_ids], year_origin[[as.character(y)]], units = "days")
    )
    dayfrac <- 1 / out_day

    bounds <- array(data = NA_real_, dim = c(length(sub_dates_cf), 2))
    bounds[, 1] <- sub_dates_cf
    bounds[, 2] <- bounds[, 1] + dayfrac
    bounds <- round(bounds, 4)

    output <- lapply(output_all, function(x) x[row_ids])
    output[["time_bounds"]] <- c(rbind(bounds[, 1], bounds[, 2]))

    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) {
        output[[i]] <- rep(-999, length(sub_dates_cf))
      }
      output[[i]][is.na(output[[i]])] <- -999
    }

    t <- ncdf4::ncdim_def(
      name = "time",
      longname = "time",
      units = paste0("days since ", y, "-01-01 00:00:00"),
      vals = sub_dates_cf,
      calendar = "standard",
      unlim = TRUE
    )
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    dims <- list(lon = lon, lat = lat, time = t)
    time_interval <- ncdf4::ncdim_def(
      name = "hist_interval",
      longname = "history time interval endpoint dimensions",
      vals = 1:2,
      units = ""
    )

    nc_var <- list(
      "GPP" = PEcAn.utils::to_ncvar("GPP", dims),
      "NPP" = PEcAn.utils::to_ncvar("NPP", dims),
      "TotalResp" = PEcAn.utils::to_ncvar("TotalResp", dims),
      "AutoResp" = PEcAn.utils::to_ncvar("AutoResp", dims),
      "HeteroResp" = PEcAn.utils::to_ncvar("HeteroResp", dims),
      "SoilResp" = ncdf4::ncvar_def("SoilResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999,
                                    longname = "Soil Respiration"),
      "NEE" = PEcAn.utils::to_ncvar("NEE", dims),
      "AbvGrndWood" = PEcAn.utils::to_ncvar("AbvGrndWood", dims),
      "leaf_carbon_content" = PEcAn.utils::to_ncvar("leaf_carbon_content", dims),
      "TotLivBiom" = PEcAn.utils::to_ncvar("TotLivBiom", dims),
      "TotSoilCarb" = PEcAn.utils::to_ncvar("TotSoilCarb", dims),
      "Qle" = PEcAn.utils::to_ncvar("Qle", dims),
      "Transp" = PEcAn.utils::to_ncvar("Transp", dims),
      "SoilMoist" = PEcAn.utils::to_ncvar("SoilMoist", dims),
      "SoilMoistFrac" = PEcAn.utils::to_ncvar("SoilMoistFrac", dims),
      "SWE" = PEcAn.utils::to_ncvar("SWE", dims),
      "litter_carbon_content" = PEcAn.utils::to_ncvar("litter_carbon_content", dims),
      "litter_mass_content_of_water" = PEcAn.utils::to_ncvar("litter_mass_content_of_water", dims),
      "LAI" = PEcAn.utils::to_ncvar("LAI", dims),
      "fine_root_carbon_content" = PEcAn.utils::to_ncvar("fine_root_carbon_content", dims),
      "coarse_root_carbon_content" = PEcAn.utils::to_ncvar("coarse_root_carbon_content", dims),
      "GWBI" = ncdf4::ncvar_def("GWBI", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                                longname = "Gross Woody Biomass Increment"),
      "AGB" = ncdf4::ncvar_def("AGB", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                               longname = "Total aboveground biomass"),
      "time_bounds" = ncdf4::ncvar_def(name = "time_bounds", units = "",
                                       longname = "history time interval endpoints", dim = list(time_interval, time = t),
                                       prec = "double")
    )

    if (file.exists(target_file)) {
      unlink(target_file)
    }

    nc <- ncdf4::nc_create(target_file, nc_var)
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec = NA)
    for (key in names(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[key]], output[[key]])
    }
    ncdf4::nc_close(nc)

    invisible(target_file)
  }

  t_year_loop <- tic()
  if (!conflict) {
    years_to_write <- Filter(function(y) {
      row_ids <- year_index[[as.character(y)]]
      if (is.null(row_ids) || length(row_ids) == 0) {
        return(FALSE)
      }
      target_file <- file.path(outdir, paste(y, "nc", sep = "."))
      if (file.exists(target_file) && !overwrite) {
        return(FALSE)
      }
      TRUE
    }, year_seq)

    if (worker_setting > 1L && length(years_to_write) > 1L) {
      parallel::mclapply(
        years_to_write,
        function(y) {
          row_ids <- year_index[[as.character(y)]]
          target_file <- file.path(outdir, paste(y, "nc", sep = "."))
          write_year_file(y, row_ids, target_file)
          NULL
        },
        mc.cores = min(worker_setting, length(years_to_write))
      )
    } else {
      for (y in years_to_write) {
        row_ids <- year_index[[as.character(y)]]
        target_file <- file.path(outdir, paste(y, "nc", sep = "."))
        write_year_file(y, row_ids, target_file)
      }
    }
  } else {
    for (y in year_seq) {
      row_ids <- year_index[[as.character(y)]]
      if (is.null(row_ids) || length(row_ids) == 0) {
        next
      }

      target_file <- file.path(outdir, paste(y, "nc", sep = "."))
      existing_file <- file.exists(target_file)

      if (existing_file && overwrite) {
        unlink(target_file)
        existing_file <- FALSE
      }

      if (existing_file) {
        current_file <- tempfile(tmpdir = outdir, pattern = paste0("current_", y, "_"), fileext = ".nc")
        merged_file <- tempfile(tmpdir = outdir, pattern = paste0("merged_", y, "_"), fileext = ".nc")

        write_year_file(y, row_ids, current_file)
        mergeNC(files = c(target_file, current_file), outfile = merged_file)

        if (file.exists(merged_file)) {
          nc <- ncdf4::nc_open(merged_file, write = TRUE)
          if ("time_bnds" %in% names(nc$var)) {
            nc <- ncdf4::ncvar_rename(nc, "time_bnds", "time_bounds")
          }
          ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec = NA)
          ncdf4::nc_close(nc)

          if (!file.rename(merged_file, target_file)) {
            copied <- file.copy(merged_file, target_file, overwrite = TRUE)
            unlink(merged_file)
            if (!copied) {
              PEcAn.logger::logger.error("Failed to replace target file after merge", target_file)
            }
          }
        }

        unlink(current_file)
      } else {
        write_year_file(y, row_ids, target_file)
      }
    }
  }
  toc("yearly_netcdf_conversion", t_year_loop)

  if (delete.raw) {
    file.remove(sipnet_out_file)
  }

  toc("model2netcdf_total", t_total)
}
#--------------------------------------------------------------------------------------------------#
### EOF
