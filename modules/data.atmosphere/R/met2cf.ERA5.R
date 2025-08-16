#' met2cf.ERA5
#'
#' @param lat latitude
#' @param long longitude
#' @param start_date start date
#' @param end_date end date
#' @param sitename The name of the site used for making the identifier.
#' @param outfolder Path to directory where nc files need to be saved.
#' @param out.xts Output of the extract.nc.ERA5 function which is a list of time series of met variables for each ensemble member.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Logical flag defining if ouput of function be extra verbose.
#'
#' @author Hamze Dokohaki, David LeBauer
#' @return list of dataframes
#' @export
#'
met2CF.ERA5<- function(lat,
                        long,
                        start_date,
                        end_date,
                        sitename,
                        outfolder,
                        out.xts,
                        overwrite = FALSE,
                        verbose = TRUE) {

  years <- seq(lubridate::year(start_date),
               lubridate::year(end_date),
               1
  )
  # Derive number of ensembles from provided list (was hard-coded 1:10)
  ensemblesN <- seq_along(out.xts)

  start_date <- paste0(lubridate::year(start_date),"-01-01")  %>% as.Date()
  end_date <- paste0(lubridate::year(end_date),"-12-31") %>% as.Date()
  # adding RH and converting rain

  # Prepare ERA5 -> CF mapping & units from standard table
  era5_tbl <- pecan_standard_met_table %>%
    dplyr::filter(!is.na(era5) & nzchar(era5))
  era5_to_cf <- setNames(era5_tbl$cf_standard_name, era5_tbl$era5)
  cf_units_map <- setNames(era5_tbl$units, era5_tbl$cf_standard_name)

  out.new <- ensemblesN %>%
    purrr::map(function(ensi) {
      ens <- out.xts[[ensi]]
      if (is.null(ens) || nrow(ens) == 0) {
        PEcAn.logger::logger.warn(paste("Empty ensemble", ensi))
        return(NULL)
      }
      # Determine timestep (median) – fallback 3h (10800s)
      dt_vec <- diff(as.numeric(zoo::index(ens)))
      dt_sec <- if (length(dt_vec)) as.numeric(median(dt_vec)) else 10800
      # Flux conversions J/m2 -> W/m2
      if ("ssrd" %in% colnames(ens)) ens[, "ssrd"] <- ens[, "ssrd"] / dt_sec
      if ("strd" %in% colnames(ens)) ens[, "strd"] <- ens[, "strd"] / dt_sec
      # Precip m -> kg m-2 s-1
      if ("tp" %in% colnames(ens))  ens[, "tp"] <- (ens[, "tp"] * 1000) / dt_sec
      # Specific humidity (needs t2m,d2m,sp)
      spec_hum <- NULL
      if (all(c("t2m","d2m","sp") %in% colnames(ens))) {
        # Vectorized RH via Magnus formula over water (Kelvin inputs)
        T_k  <- as.numeric(ens[, "t2m"])   # K
        Td_k <- as.numeric(ens[, "d2m"])   # K
        T_c  <- T_k  - 273.15
        Td_c <- Td_k - 273.15
        es <- 6.112 * exp((17.62 * T_c)  / (243.12 + T_c))    # hPa
        e  <- 6.112 * exp((17.62 * Td_c) / (243.12 + Td_c))   # hPa
        rh_prop <- pmin(pmax(e / es, 0), 1)                   # [0,1]
        spec_vals <- PEcAn.data.atmosphere::rh2qair(rh_prop, T_k, as.numeric(ens[, "sp"]))
        spec_hum <- xts::xts(spec_vals, order.by = zoo::index(ens))
        colnames(spec_hum) <- "specific_humidity"
      }
      native_vars <- intersect(names(era5_to_cf), colnames(ens))
      if (!length(native_vars)) {
        PEcAn.logger::logger.warn("No mappable ERA5 vars in ensemble member.")
        return(NULL)
      }
      cf_data <- ens[, native_vars, drop = FALSE]
      colnames(cf_data) <- era5_to_cf[native_vars]
      if (!is.null(spec_hum)) cf_data <- xts::merge.xts(cf_data, spec_hum)
      cf_data
    })

  # Filter NULL ensembles (if any)
  valid_idx <- which(!vapply(out.new, is.null, logical(1)))
  if (!length(valid_idx)) {
    PEcAn.logger::logger.severe("No valid ensemble data after processing.")
    return(list())
  }
  out.new <- out.new[valid_idx]
  ensemblesN <- seq_along(out.new)

  # CF variable names to write and their units (guarantee equal lengths)
  cf_var_names <- colnames(out.new[[1]])
  cf_var_units <- purrr::map_chr(cf_var_names, function(nm) {
    u <- unname(cf_units_map[nm])
    if (length(u) == 0 || is.na(u)) {
      if (identical(nm, "specific_humidity")) return("1")  # unitless (mass ratio)
      return(NA_character_)
    }
    as.character(u)
  })
  names(cf_var_units) <- cf_var_names


  results_list <-  ensemblesN %>%
    purrr::map(function(i) {

      start_date <- min(zoo::index(out.new[[i]]))
      end_date <- max(zoo::index(out.new[[i]]))
      # Create a data frame with metadata and file info.
      results <- data.frame(
        file = "",
        #Path to the file (added in loop below).
        host = PEcAn.remote::fqdn(),
        mimetype = "application/x-netcdf",
        formatname = "CF Meteorology",
        startdate = paste0(format(
          start_date , "%Y-%m-%dT%H:%M:00 %z"
        )),
        enddate = paste0(format(
          end_date , "%Y-%m-%dT%H:%M:00 %z"
        )),
        dbfile.name = paste0("ERA5.", i),
        stringsAsFactors = FALSE
      )

      # i is the ensemble number
      #Generating a unique identifier string that characterizes a particular data set.
      identifier <- paste("ERA5", sitename, i, sep = "_")

      identifier.file <- paste("ERA5",
                               i,
                               lubridate::year(start_date),
                               sep = ".")
      
      ensemble_folder <- file.path(outfolder, identifier)
      
      #Each file will go in its own folder.
      if (!dir.exists(ensemble_folder)) {
        dir.create(ensemble_folder,
                   recursive = TRUE,
                   showWarnings = FALSE)
      }
      
      flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
      
      #Each ensemble member gets its own unique data frame, which is stored in results_list
      results$file <- flname
      
      years %>%
        purrr::map(function(year) {
          #
          identifier.file <- paste("ERA5",
                                   i,
                                   year,
                                   sep = ".")
          
          flname <-file.path(ensemble_folder, paste(identifier.file, "nc", sep = "."))
          # Spliting it for this year
          data.for.this.year.ens <- out.new[[i]]
          data.for.this.year.ens <- data.for.this.year.ens[year %>% as.character]
          
          # Build time coordinate from actual timestamps (hours since start_date)
          hrs_since_start <- as.numeric(difftime(zoo::index(data.for.this.year.ens),
                                                 start_date, units = "hours"))
          time_dim = ncdf4::ncdim_def("time",
                                      paste("hours since", format(start_date, "%Y-%m-%d %H:%M:%S")),
                                      vals = hrs_since_start,
                                      create_dimvar = TRUE)
          lat_dim = ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
          lon_dim = ncdf4::ncdim_def("longitude", "degree_east", long, create_dimvar = TRUE)
          
          #create a list of all ens
          nc_var_list <- purrr::map2(cf_var_names,
                                     cf_var_units,
                                     ~ ncdf4::ncvar_def(.x, .y, list(time_dim, lat_dim, lon_dim), missval = NA_real_))
       
          #results$dbfile.name <- flname
          
          
          if (!file.exists(flname) || overwrite) {
            tryCatch({
              nc_flptr <- ncdf4::nc_create(flname, nc_var_list, verbose = verbose)
              
              #For each variable associated with that ensemble
              for (j in seq_along(cf_var_names)) {
                # "j" is the variable number.  "i" is the ensemble number.
                ncdf4::ncvar_put(nc_flptr,
                                 nc_var_list[[j]],
                                 zoo::coredata(data.for.this.year.ens)[, nc_var_list[[j]]$name])
              }
              
              ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
            },
            error = function(e) {
              PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                          conditionMessage(e))
            })
            
          } else {
            PEcAn.logger::logger.info(paste0(
              "The file ",
              flname,
              " already exists.  It was not overwritten."
            ))
          }
          
          
        }) 
      
      return(results)
    })
  #For each ensemble
  return(results_list )
}
