##' Writes a configuration files for your model
##' @name write.config.SIPNET
##' @title Writes a configuration files for SIPNET model
##' @param defaults pft
##' @param trait.values vector of samples for a given trait
##' @param settings PEcAn settings object
##' @param run.id run ID
##' @param inputs list of model inputs
##' @param IC initial condition
##' @param restart In case this is a continuation of an old simulation. restart needs to be a list with name tags of runid, inputs, new.params (parameters), new.state (initial condition), ensemble.id (ensemble id), start.time and stop.time.See Details.
##' @param spinup currently unused, included for compatibility with other models
##' @export
##' @importFrom rlang %||%
##' @author Michael Dietze
.sipnet_config_cache <- new.env(parent = emptyenv())

.sipnet_profile_enabled <- function() {
  tolower(trimws(Sys.getenv("PECAN_SIPNET_PROFILE", ""))) %in% c("1", "true", "yes", "on")
}

.sipnet_verbose_enabled <- function() {
  tolower(trimws(Sys.getenv("PECAN_SIPNET_VERBOSE", ""))) %in% c("1", "true", "yes", "on")
}

.sipnet_cache_read_lines <- function(path) {
  key <- paste0("lines::", normalizePath(path, mustWork = FALSE))
  if (!exists(key, envir = .sipnet_config_cache, inherits = FALSE)) {
    assign(key, readLines(con = path, n = -1), envir = .sipnet_config_cache)
  }
  get(key, envir = .sipnet_config_cache, inherits = FALSE)
}

.sipnet_cache_read_table <- function(path) {
  key <- paste0("table::", normalizePath(path, mustWork = FALSE))
  if (!exists(key, envir = .sipnet_config_cache, inherits = FALSE)) {
    assign(key, utils::read.table(path), envir = .sipnet_config_cache)
  }
  get(key, envir = .sipnet_config_cache, inherits = FALSE)
}

write.config.SIPNET <- function(defaults, trait.values, settings, run.id, inputs = NULL, IC = NULL,
                                restart = NULL, spinup = NULL) {
  profile_enabled <- .sipnet_profile_enabled()
  timings <- list()
  tic <- function() proc.time()[["elapsed"]]
  toc <- function(stage, started_at) {
    if (!profile_enabled) {
      return(invisible(NULL))
    }
    timings[[length(timings) + 1]] <<- data.frame(
      function_name = "write.config.SIPNET",
      run_id = as.character(run.id),
      stage = stage,
      elapsed_seconds = unname(proc.time()[["elapsed"]] - started_at),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  ### WRITE sipnet.in
  t_main <- tic()
  template.in <- system.file("sipnet.in", package = "PEcAn.SIPNET")
  config.text <- .sipnet_cache_read_lines(template.in)
  writeLines(config.text, con = file.path(settings$rundir, run.id, "sipnet.in"))
  
  ### WRITE *.clim
  template.clim <- settings$run$inputs$met$path  ## read from settings
  if (!is.null(inputs)) {
    ## override if specified in inputs
    if ("met" %in% names(inputs)) {
      template.clim <- inputs$met$path
    }
  }
  if (.sipnet_verbose_enabled()) {
    PEcAn.logger::logger.info(paste0("Writing SIPNET configs with input ", template.clim))
  }
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, as.character(run.id))
  outdir <- file.path(settings$host$outdir, as.character(run.id))
  if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }

  write_profile_csv <- trimws(Sys.getenv("PECAN_SIPNET_PROFILE_CSV", ""))
  write_profile_csv <- if (profile_enabled) {
    if (tolower(write_profile_csv) %in% c("1", "true", "yes", "on")) {
      file.path(outdir, "sipnet_write.config.profile.csv")
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
      paste0(x[["function_name"]], " [", x[["run_id"]], "] ", x[["stage"]], ": ", signif(as.numeric(x[["elapsed_seconds"]]), 4), " s")
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
  
  # create launch script (which will create symlink)
  t_jobsh <- tic()
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- .sipnet_cache_read_lines(settings$model$jobtemplate)
  } else {
    jobsh <- .sipnet_cache_read_lines(system.file("template.job", package = "PEcAn.SIPNET"))
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  # create cdo specific settings
  cdosetup <- ""
  if (!is.null(settings$host$cdosetup)) {
    cdosetup <- paste(cdosetup, sep = "\n", paste(settings$host$cdosetup, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create rabbitmq specific setup.
  cpruncmd <- cpoutcmd <- rmoutdircmd <- rmrundircmd <- ""
  if (!is.null(settings$host$rabbitmq)) {
    #rsync cmd from remote to local host.
    settings$host$rabbitmq$cpfcmd <- ifelse(is.null(settings$host$rabbitmq$cpfcmd), "", settings$host$rabbitmq$cpfcmd)
    cpruncmd <- gsub("@OUTDIR@", settings$host$rundir, settings$host$rabbitmq$cpfcmd)
    cpruncmd <- gsub("@OUTFOLDER@", rundir, cpruncmd)
    
    cpoutcmd <- gsub("@OUTDIR@", settings$host$outdir, settings$host$rabbitmq$cpfcmd)
    cpoutcmd <- gsub("@OUTFOLDER@", outdir, cpoutcmd)
    
    #delete files within rundir and outdir.
    rmoutdircmd <- paste("rm", file.path(outdir, "*"))
    rmrundircmd <- paste("rm", file.path(rundir, "*"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@CDO_SETUP@", cdosetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", template.clim, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@",settings$run$end.date , jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@REVISION@", settings$model$revision, jobsh)
  
  jobsh <- gsub("@CPRUNCMD@", cpruncmd, jobsh)
  jobsh <- gsub("@CPOUTCMD@", cpoutcmd, jobsh)
  jobsh <- gsub("@RMOUTDIRCMD@", rmoutdircmd, jobsh)
  jobsh <- gsub("@RMRUNDIRCMD@", rmrundircmd, jobsh)
  
  if(is.null(settings$state.data.assimilation$NC.Prefix)){
    settings$state.data.assimilation$NC.Prefix <- "sipnet.out"
  }
  jobsh <- gsub("@PREFIX@", settings$state.data.assimilation$NC.Prefix, jobsh)
  
  #overwrite argument
  if(is.null(settings$state.data.assimilation$NC.Overwrite)){
    settings$state.data.assimilation$NC.Overwrite <- FALSE
  }
  jobsh <- gsub("@OVERWRITE@", settings$state.data.assimilation$NC.Overwrite, jobsh)
  
  #allow conflict? meaning allow full year nc export.
  if(is.null(settings$state.data.assimilation$FullYearNC)){
    settings$state.data.assimilation$FullYearNC <- FALSE
  }
  jobsh <- gsub("@CONFLICT@", settings$state.data.assimilation$FullYearNC, jobsh)
  
  if (is.null(settings$model$delete.raw)) {
    settings$model$delete.raw <- FALSE
  }
  jobsh <- gsub("@DELETE.RAW@", settings$model$delete.raw, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  toc("write_job_sh", t_jobsh)
  

  ### Copy event file
  event_file <- inputs$events$path %||% settings$run$inputs$events$path
  if (!is.null(event_file)) {
    if (!file.exists(event_file)) {
      PEcAn.logger::logger.warn("Event file not found at", event_file)
    }
    file.copy(event_file, file.path(rundir, "events.in"))
  }


  ### WRITE *.param-spatial
  template.paramSpatial <- system.file("template.param-spatial", package = "PEcAn.SIPNET")
  file.copy(template.paramSpatial, file.path(settings$rundir, run.id, "sipnet.param-spatial"))
  
  ### WRITE *.param
  t_param <- tic()
  template.param <- system.file("template.param", package = "PEcAn.SIPNET")
  if ("default.param" %in% names(settings$model)) {
    template.param <- settings$model$default.param
  }
  
  param <- .sipnet_cache_read_table(template.param)
  param_index <- as.list(seq_len(nrow(param)))
  names(param_index) <- param[, 1]
  idx <- function(name) {
    out <- param_index[[name]]
    if (is.null(out)) {
      PEcAn.logger::logger.error("Missing parameter", sQuote(name), "in SIPNET template")
    }
    out
  }
  
  #### write run-specific PFT parameters here
  #
  # Q: "Wait, Sipnet only uses one PFT at a time. What's this loop doing?"
  #
  # A: Sipnet only uses one *vegetation* PFT at a time, but this hack lets us
  #    also pass a "soil PFT" of values for a suite of biogeochemical traits.
  #   We do check that each trait appears in only one PFT (so that the loop
  #    sets each parameter no more than one time), but it is up to the user to
  #    confirm whether the resulting joint parameter set makes any sense.
  # TODO: consider flattening trait.values to eliminate the loop entirely?
  #   Might be as simple as (untested!)
  #   trait.values <- Reduce(trait.values, f=append)
  trait_names_all_pfts <- as.vector(sapply(trait.values, names))
  dup_traitnames <- trait_names_all_pfts[duplicated(trait_names_all_pfts)]
  if (length(dup_traitnames) > 0) {
    PEcAn.logger::logger.warn(
      "Multiple trait values given for parameters",
      paste(dQuote(dup_traitnames), collapse = ", "),
      "write.config.SIPNET will use the value it sees last."
    )
  }
  constant.traits <- unlist(defaults[[1]]$constants)
  constant.names <- names(constant.traits)

  leafphdata <- NULL
  obs_year_start <- NA_integer_
  obs_year_end <- NA_integer_
  if (!is.null(settings$run$inputs$leaf_phenology)) {
    obs_year_start <- lubridate::year(settings$run$start.date)
    obs_year_end <- lubridate::year(settings$run$end.date)
    if (obs_year_start != obs_year_end) {
      PEcAn.logger::logger.info(
        "Start.date and end.date are not in the same year.",
        "Using phenological data from start year only."
      )
    }
    leaf_pheno_path <- settings$run$inputs$leaf_phenology$path
    if (!is.null(leaf_pheno_path)) {
      leafphdata <- utils::read.csv(leaf_pheno_path)
    } else {
      PEcAn.logger::logger.info("No phenology data were found.",
        "Please consider running `PEcAn.data.remote::extract_phenology_MODIS`",
        "to get the parameter file."
      )
    }
  }

  for (pft in seq_along(trait.values)) {
    pft.traits <- unlist(trait.values[[pft]])
    pft.trait.names <- names(pft.traits)
    
    ## Append/replace params specified as constants
    # Replace matches
    for (i in seq_along(constant.traits)) {
      ind <- match(constant.names[i], pft.trait.names)
      if (is.na(ind)) {
        # Add to list
        pft.trait.names <- c(pft.trait.names, constant.names[i])
        pft.traits <- c(pft.traits, constant.traits[i])
      } else {
        # Replace existing value
        pft.traits[ind] <- constant.traits[i]
      }
    }
    
    # Remove NAs. Constants may be specified as NA to request template defaults. Note that it is 'NA'
    # (character) not actual NA due to being read in as XML
    pft.trait.names <- pft.trait.names[pft.traits != "NA" & !is.na(pft.traits)]
    pft.traits <- pft.traits[pft.traits != "NA" & !is.na(pft.traits)]
    pft.traits <- as.numeric(pft.traits)

    # Leaf carbon concentration
    if ("leafC" %in% pft.trait.names) {
      leafC <- pft.traits[pft.trait.names == "leafC"] |>
        PEcAn.utils::ud_convert("percent", "1") # percentage to fraction
      id <- idx("cFracLeaf")
      param[id, 2] <- leafC
    } else {
      leafC <- 0.48 # Fixed value if not available, because it is used in downstream calculations
    }

    # Specific leaf area converted to SLW
    # leafCSpWt [gC/m2 leaf], SLA [m2 leaf/kg leaf], leafC [g C / g leaf]
    id <- idx("leafCSpWt")
    if ("SLA" %in% pft.trait.names) {
      SLA <- pft.traits[which(pft.trait.names == "SLA")]
      param[id, 2] <- PEcAn.utils::ud_convert(leafC / SLA, "kg/m2", "g/m2")
    } else {
      SLA <- PEcAn.utils::ud_convert(leafC / param[id, 2], "m2/g", "m2/kg")
    }

    # Maximum photosynthesis
    # SIPNET: aMax [nmol CO2 / g   leaf / sec]
    # PEcAn:  Amax [umol CO2 / m^2 leaf / sec]
    id <- idx("aMax")
    SLA_g <- PEcAn.utils::ud_convert(SLA, "1/kg", "1/g") 
    if ("Amax" %in% pft.trait.names) {
      Amax_area <- pft.traits[which(pft.trait.names == "Amax")] # [µmol/m2/s]
      param[id, 2] <- PEcAn.utils::ud_convert(Amax_area * SLA_g, "umol", "nmol") # [nmol/g/s]
    } else {
      amax_mass <- param[id, 2] # [nmol/g/s]
      Amax_area <- PEcAn.utils::ud_convert(amax_mass / SLA_g, "nmol", "umol") # [umol/m2/s]
    }
    
    # Daily fraction of maximum photosynthesis
    if ("AmaxFrac" %in% pft.trait.names) {
      param[idx("aMaxFrac"), 2] <- pft.traits[which(pft.trait.names == "AmaxFrac")]
    }
    
    ### Canopy extinction coefficiet (k)
    if ("extinction_coefficient" %in% pft.trait.names) {
      param[idx("attenuation"), 2] <- pft.traits[which(pft.trait.names == "extinction_coefficient")]
    }
    
    # Leaf respiration rate converted to baseFolRespFrac
    if ("leaf_respiration_rate_m2" %in% pft.trait.names) {
      Rd <- pft.traits[which(pft.trait.names == "leaf_respiration_rate_m2")]
      id <- idx("baseFolRespFrac")
      param[id, 2] <- max(min(Rd / Amax_area, 1), 0)
    }
    
    # Low temp threshold for photosynethsis
    if ("Vm_low_temp" %in% pft.trait.names) {
      param[idx("psnTMin"), 2] <- pft.traits[which(pft.trait.names == "Vm_low_temp")]
    }
    
    # Opt. temp for photosynthesis
    if ("psnTOpt" %in% pft.trait.names) {
      param[idx("psnTOpt"), 2] <- pft.traits[which(pft.trait.names == "psnTOpt")]
    }
    
    # Growth respiration factor (fraction of GPP)
    if ("growth_resp_factor" %in% pft.trait.names) {
      param[idx("growthRespFrac"), 2] <- pft.traits[which(pft.trait.names == "growth_resp_factor")]
    }
    ### !!! NOT YET USED
    #Jmax = NA
    #if("Jmax" %in% pft.trait.names){
    #  Jmax = pft.traits[which(pft.trait.names == 'Jmax')]
    ### Using Jmax scaled to 25 degC. Maybe not be the best approach
    #}
    
    #alpha = NA
    #if("quantum_efficiency" %in% pft.trait.names){
    #  alpha = pft.traits[which(pft.trait.names == 'quantum_efficiency')]
    #}
    
    # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 theoretical maximum (Einsteins * m^-2 ground area * day^-1).
    #if(!is.na(Jmax) & !is.na(alpha)){
    # param[idx("halfSatPar"),2] = Jmax/(2*alpha)
    ### WARNING: this is a very coarse linear approximation and needs improvement *****
    ### Yes, we also need to work on doing a paired query where we have both data together.
    ### Once halfSatPar is calculated, need to remove Jmax and quantum_efficiency from param list so they are not included in SA
    #}
    ### !!!
    
    # Half saturation of PAR.  PAR at which photosynthesis occurs at 1/2 theoretical maximum (Einsteins * m^-2 ground area * day^-1).
    # Temporary implementation until above is working.
    if ("half_saturation_PAR" %in% pft.trait.names) {
      param[idx("halfSatPar"), 2] <- pft.traits[which(pft.trait.names == "half_saturation_PAR")]
    }
    
    # Ball-berry slomatal slope parameter m
    if ("stomatal_slope.BB" %in% pft.trait.names) {
      id <- idx("m_ballBerry")
      param[id, 2] <- pft.traits[which(pft.trait.names == "stomatal_slope.BB")]
    }
    
    # Slope of VPD–photosynthesis relationship. dVpd = 1 - dVpdSlope * vpd^dVpdExp
    if ("dVPDSlope" %in% pft.trait.names) {
      param[idx("dVpdSlope"), 2] <- pft.traits[which(pft.trait.names == "dVPDSlope")]
    }
    
    # VPD–water use efficiency relationship.  dVpd = 1 - dVpdSlope * vpd^dVpdExp
    if ("dVpdExp" %in% pft.trait.names) {
      param[idx("dVpdExp"), 2] <- pft.traits[which(pft.trait.names == "dVpdExp")]
    }
    
    # Leaf turnover rate average turnover rate of leaves, in fraction per day NOTE: read in as
    # per-year rate!
    if ("leaf_turnover_rate" %in% pft.trait.names) {
      param[idx("leafTurnoverRate"), 2] <- pft.traits[which(pft.trait.names == "leaf_turnover_rate")]
    }
    
    if ("wueConst" %in% pft.trait.names) {
      param[idx("wueConst"), 2] <- pft.traits[which(pft.trait.names == "wueConst")]
    }
    # vegetation respiration Q10.
    if ("veg_respiration_Q10" %in% pft.trait.names) {
      param[idx("vegRespQ10"), 2] <- pft.traits[which(pft.trait.names == "veg_respiration_Q10")]
    }
    
    # Base vegetation respiration. vegetation maintenance respiration at 0 degrees C (g C respired * g^-1 plant C * day^-1)
    # NOTE: only counts plant wood C - leaves handled elsewhere (both above and below-ground: assumed for now to have same resp. rate)
    # NOTE: read in as per-year rate!
    if ("stem_respiration_rate" %in% pft.trait.names) {
      vegRespQ10 <- param[idx("vegRespQ10"), 2]
      id <- idx("baseVegResp")
      ## Convert from umols CO2 kg s-1 to gC g day-1
      stem_resp_g <- (((pft.traits[which(pft.trait.names == "stem_respiration_rate")]) *
                         (44.0096 / 1e+06) * (12.01 / 44.0096)) / 1000) * 86400
      ## use Q10 to convert stem resp from reference of 25C to 0C param[id,2] =
      ## pft.traits[which(pft.trait.names=='stem_respiration_rate')]*vegRespQ10^(-25/10)
      param[id, 2] <- stem_resp_g * vegRespQ10^(-25/10)
    }
    
    # turnover of fine roots (per year rate)
    if ("root_turnover_rate" %in% pft.trait.names) {
      id <- idx("fineRootTurnoverRate")
      param[id, 2] <- pft.traits[which(pft.trait.names == "root_turnover_rate")]
    }
    
    # fine root respiration Q10
    if ("fine_root_respiration_Q10" %in% pft.trait.names) {
      param[idx("fineRootQ10"), 2] <- pft.traits[which(pft.trait.names == "fine_root_respiration_Q10")]
    }
    
    # base respiration rate of fine roots (per year rate)
    if ("root_respiration_rate" %in% pft.trait.names) {
      fineRootQ10 <- param[idx("fineRootQ10"), 2]
      id <- idx("baseFineRootResp")
      ## Convert from umols CO2 kg s-1 to gC g day-1
      root_resp_rate_g <- (((pft.traits[which(pft.trait.names == "root_respiration_rate")]) *
                              (44.0096/1e+06) * (12.01 / 44.0096)) / 1000) * 86400
      ## use Q10 to convert stem resp from reference of 25C to 0C param[id,2] =
      ## pft.traits[which(pft.trait.names=='root_respiration_rate')]*fineRootQ10^(-25/10)
      param[id, 2] <- root_resp_rate_g * fineRootQ10 ^ (-25 / 10)
    }
    
    # coarse root respiration Q10
    if ("coarse_root_respiration_Q10" %in% pft.trait.names) {
      param[idx("coarseRootQ10"), 2] <- pft.traits[which(pft.trait.names == "coarse_root_respiration_Q10")]
    }
    # WARNING: fineRootAllocation + woodAllocation + leafAllocation isn't supposed to exceed 1
    # see sipnet.c code L2005 :
    # fluxes.coarseRootCreation=(1-params.leafAllocation-params.fineRootAllocation-params.woodAllocation)*npp;
    # priors can be chosen accordingly, and SIPNET doesn't really crash when sum>1 but better keep an eye
    alloc_params <- c("root_allocation_fraction", "wood_allocation_fraction", "leaf_allocation_fraction")
    if (all(alloc_params %in% pft.trait.names)) {
      sum_alloc <- pft.traits[which(pft.trait.names == "root_allocation_fraction")] +
        pft.traits[which(pft.trait.names == "wood_allocation_fraction")] +
        pft.traits[which(pft.trait.names == "leaf_allocation_fraction")]
      if(sum_alloc > 1){
        # I want this to be a severe for now, lateer can be changed back to warning
        PEcAn.logger::logger.warn("Sum of allocation parameters exceeds 1 for runid = ", run.id,
                                  "- This won't break anything since SIPNET has internal check, but notice that such combinations might not take effect in the outputs.")
      }
    }
    
    
    # fineRootAllocation
    if ("root_allocation_fraction" %in% pft.trait.names) {
      param[idx("fineRootAllocation"), 2] <- pft.traits[which(pft.trait.names == "root_allocation_fraction")]
    }
    
    # woodAllocation
    if ("wood_allocation_fraction" %in% pft.trait.names) {
      param[idx("woodAllocation"), 2] <- pft.traits[which(pft.trait.names == "wood_allocation_fraction")]
    }
    
    # leafAllocation
    if ("leaf_allocation_fraction" %in% pft.trait.names) {
      param[idx("leafAllocation"), 2] <- pft.traits[which(pft.trait.names == "leaf_allocation_fraction")]
    }
    
    # wood_turnover_rate
    if ("wood_turnover_rate" %in% pft.trait.names) {
      param[idx("woodTurnoverRate"), 2] <- pft.traits[which(pft.trait.names == "wood_turnover_rate")]
    }
    
    ### ----- Soil parameters soil respiration Q10.
    if ("soil_respiration_Q10" %in% pft.trait.names) {
      param[idx("soilRespQ10"), 2] <- pft.traits[which(pft.trait.names == "soil_respiration_Q10")]
    }
    # soil respiration rate -- units = 1/year, reference = 0C
    if ("som_respiration_rate" %in% pft.trait.names) {
      param[idx("baseSoilResp"), 2] <- pft.traits[which(pft.trait.names == "som_respiration_rate")]
    }
    
    # litterBreakdownRate
    if ("turn_over_time" %in% pft.trait.names) {
      id <- idx("litterBreakdownRate")
      param[id, 2] <- pft.traits[which(pft.trait.names == "turn_over_time")]
    }
    # frozenSoilEff
    if ("frozenSoilEff" %in% pft.trait.names) {
      param[idx("frozenSoilEff"), 2] <- pft.traits[which(pft.trait.names == "frozenSoilEff")]
    }
    
    # frozenSoilFolREff
    if ("frozenSoilFolREff" %in% pft.trait.names) {
      param[idx("frozenSoilFolREff"), 2] <- pft.traits[which(pft.trait.names == "frozenSoilFolREff")]
    }
    
    # soilWHC
    if ("soilWHC" %in% pft.trait.names) {
      param[idx("soilWHC"), 2] <- pft.traits[which(pft.trait.names == "soilWHC")]
    }
    # 10/31/2017 IF: these were the two assumptions used in the emulator paper in order to reduce dimensionality
    # These results in improved winter soil respiration values
    # they don't affect anything when the seasonal soil respiration functionality in SIPNET is turned-off
    if(TRUE){
      # assume soil resp Q10 cold == soil resp Q10
      param[idx("soilRespQ10Cold"), 2] <- param[idx("soilRespQ10"), 2]
      # default SIPNET prior of baseSoilRespCold was 1/4th of baseSoilResp
      # assuming they will scale accordingly
      param[idx("baseSoilRespCold"), 2] <- param[idx("baseSoilResp"), 2] * 0.25
    }
    
    if ("immedEvapFrac" %in% pft.trait.names) {
      param[idx("immedEvapFrac"), 2] <- pft.traits[which(pft.trait.names == "immedEvapFrac")]
    }
    
    if ("leafWHC" %in% pft.trait.names) {
      param[idx("leafPoolDepth"), 2] <- pft.traits[which(pft.trait.names == "leafWHC")]
    }
    
    if ("waterRemoveFrac" %in% pft.trait.names) {
      param[idx("waterRemoveFrac"), 2] <- pft.traits[which(pft.trait.names == "waterRemoveFrac")]
    }
    
    if ("fastFlowFrac" %in% pft.trait.names) {
      param[idx("fastFlowFrac"), 2] <- pft.traits[which(pft.trait.names == "fastFlowFrac")]
    }
    
    if ("rdConst" %in% pft.trait.names) {
      param[idx("rdConst"), 2] <- pft.traits[which(pft.trait.names == "rdConst")]
    }
    ### ----- Phenology parameters GDD leaf on
    if ("GDD" %in% pft.trait.names) {
      param[idx("gddLeafOn"), 2] <- pft.traits[which(pft.trait.names == "GDD")]
    }
    
    # Fraction of leaf fall per year (should be 1 for decid)
    if ("fracLeafFall" %in% pft.trait.names) {
      param[idx("fracLeafFall"), 2] <- pft.traits[which(pft.trait.names == "fracLeafFall")]
    }
    
    # Leaf growth.  Amount of C added to the leaf during the greenup period
    if ("leafGrowth" %in% pft.trait.names) {
      param[idx("leafGrowth"), 2] <- pft.traits[which(pft.trait.names == "leafGrowth")]
    }

    # update LeafOnday and LeafOffDay
    if (!is.null(leafphdata)) {
        leafOnDay <- leafphdata$leafonday[leafphdata$year == obs_year_start
                                          & leafphdata$site_id == settings$run$site$id]
        leafOffDay <- leafphdata$leafoffday[leafphdata$year == obs_year_start
                                            & leafphdata$site_id == settings$run$site$id]
        # when we have NAs for phenology (or missing years)
        if (length(leafOnDay) == 0 || is.na(leafOnDay)) {
          # 1. Try to calculate the mean across all available years for this site
          site_phenology_on <- leafphdata$leafonday[leafphdata$site_id == settings$run$site$id]
          mean_on <- mean(site_phenology_on, na.rm = TRUE)
          
          if (!is.nan(mean_on) && !is.na(mean_on)) {
            leafOnDay <- round(mean_on)
            PEcAn.logger::logger.info(paste("Missing leafOnDay for current year. Using site mean:", leafOnDay))
          } else {
            # 2. If no site history exists, fall back to parameter file
            leafOnDay <- param[idx("leafOnDay"), 2]
            PEcAn.logger::logger.warn("Missing leafOnDay and no site history. Using parameter file default.")
          }
        }
        
        if (length(leafOffDay) == 0 || is.na(leafOffDay)) {
          # 1. Try to calculate the mean across all available years for this site
          site_phenology_off <- leafphdata$leafoffday[leafphdata$site_id == settings$run$site$id]
          mean_off <- mean(site_phenology_off, na.rm = TRUE)
          
          if (!is.nan(mean_off) && !is.na(mean_off)) {
            leafOffDay <- round(mean_off)
            PEcAn.logger::logger.info(paste("Missing leafOffDay for current year. Using site mean:", leafOffDay))
          } else {
            # 2. If no site history exists, fall back to parameter file
            leafOffDay <- param[idx("leafOffDay"), 2]
            PEcAn.logger::logger.warn("Missing leafOffDay and no site history. Using parameter file default.")
          }
        }
        
        # when we have Leaf off date larger than leaf on date.
        # Otherwise the phenology will not be used.
        if (leafOffDay > leafOnDay) {
          param[idx("leafOnDay"), 2] <- leafOnDay
          param[idx("leafOffDay"), 2] <- leafOffDay
        }
    }
  } ## end loop over PFTS
  toc("update_params_from_traits", t_param)
  ####### end parameter update

  t_state_inputs <- tic()
  #working on reading soil file
  if (length(settings$run$inputs$soil_physics$path) > 0) {
    template.soil_physics <- settings$run$inputs$soil_physics$path  ## read from settings
    
    if (!is.null(inputs)) {
      ## override if specified in inputs
      if ("soil_physics" %in% names(inputs)) {
        template.soil_physics <- inputs$soil_physics$path
      }
    }
    
    if (length(template.soil_physics)!=1) {
      PEcAn.logger::logger.warn(
        paste0("No single soil physical parameter file was found for ",
               run.id))
    } else {
      soil_IC_list <- PEcAn.data.land::pool_ic_netcdf2list(template.soil_physics)
      #SoilWHC
      if ("volume_fraction_of_water_in_soil_at_saturation" %in% names(soil_IC_list$vals)) {
        #if depth is provided in the file
        if ("depth" %in% names(soil_IC_list$dims)) {
          # reduce estimates to the pre-defined soil depth.
          if (!is.null(settings$run$inputs$soil_physics$soil_depth)) {
            inds.depth <- which(soil_IC_list$dims$depth <= as.numeric(settings$run$inputs$soil_physics$soil_depth))
            soil_IC_list$dims$depth <- soil_IC_list$dims$depth[inds.depth]
            for (soil.val in names(soil_IC_list$vals)) {
              soil_IC_list$vals[[soil.val]] <- soil_IC_list$vals[[soil.val]][inds.depth]
            }
          }
          # Calculate the thickness of soil layers based on the assumption that the depth values are at bottoms and the first layer top is at 0
          thickness<-c(soil_IC_list$dims$depth[1],diff(soil_IC_list$dims$depth))
          thickness<-PEcAn.utils::ud_convert(thickness, "m", "cm")
          # Calculate the soilWHC for the whole soil profile in cm
          soilWHC_total <- sum(unlist(soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"])*thickness)
          if (thickness[1]<=10) {
            #LitterWHC in cm, assuming the litter depth is within the top 10 cm
            param[idx("litterWHC"), 2] <- unlist(soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"])[1]*thickness[1]
          }
        } else {
          #if no depth/thickness is provided
          PEcAn.logger::logger.warn("No depth info was found in the soil file. Will use the default or user-specified soil depth")
          thickness <- 100 #assume the default soil depth is the plant rooting depth of 100 cm, or use the user-specified value
          soilWHC_total <- soil_IC_list$vals["volume_fraction_of_water_in_soil_at_saturation"]*thickness
        }
        param[idx("soilWHC"), 2] <- soilWHC_total
      }
      if ("soil_hydraulic_conductivity_at_saturation" %in% names(soil_IC_list$vals)) {
         #litwaterDrainrate in cm/day
         param[idx("litWaterDrainRate"), 2] <- PEcAn.utils::ud_convert(unlist(soil_IC_list$vals["soil_hydraulic_conductivity_at_saturation"])[1], "m s-1", "cm day-1")
       }
    }
  }
  if (!is.null(IC)) {
    ic.names <- names(IC)
    ## plantWoodInit gC/m2
    plant_wood_vars <- c("AbvGrndWood", "abvGrndWoodFrac", "coarseRootFrac", "fineRootFrac")
    if (all(plant_wood_vars %in% ic.names)) {
      # reconstruct total wood C
      if(IC$abvGrndWoodFrac < 0.05){
        wood_total_C <- IC$AbvGrndWood
      }else{
        wood_total_C <- IC$AbvGrndWood / IC$abvGrndWoodFrac
      }

      #Sanity check
      if (is.infinite(wood_total_C) | is.nan(wood_total_C) | wood_total_C < 0) {
        wood_total_C <- 0
        if (round(IC$AbvGrndWood) > 0 & round(IC$abvGrndWoodFrac, 3) == 0)
          PEcAn.logger::logger.warn(
            paste0(
              "There is a major problem with ",
              run.id,
              " in either the model's parameters or IC.",
              "Because the ABG is estimated=",
              IC$AbvGrndWood,
              " while AGB Frac is estimated=",
              IC$abvGrndWoodFrac
            )
          )
        }
      param[idx("plantWoodInit"),  2] <- wood_total_C
      param[idx("coarseRootFrac"), 2] <- IC$coarseRootFrac
      param[idx("fineRootFrac"),   2] <- IC$fineRootFrac
    }
    ## laiInit m2/m2
    if ("lai" %in% ic.names) {
      param[idx("laiInit"), 2] <- IC$lai
    }
    ## litterInit gC/m2
    if ("litter_carbon_content" %in% ic.names) {
      param[idx("litterInit"), 2] <- IC$litter_carbon_content
    }
    ## soilInit gC/m2
    if ("soil" %in% ic.names) {
      param[idx("soilInit"), 2] <- IC$soil
    }
    ## litterWFracInit fraction
    if ("litter_mass_content_of_water" %in% ic.names) {
      #here we use litterWaterContent/litterWHC to calculate the litterWFracInit
      param[idx("litterWFracInit"), 2] <- IC$litter_mass_content_of_water/(param[idx("litterWHC"), 2]*10)
    }
    ## soilWater IC$soilWater is in kg/m2, and soilWHC is in cm
    if ("soilWater" %in% ic.names) {
      param[idx("soilWFracInit"), 2] <- IC$soilWater/(param[idx("soilWHC"), 2]*10)
    }
    ## soilWFracInit fraction
    if ("soilWFrac" %in% ic.names) {
      param[idx("soilWFracInit"), 2] <- IC$soilWFrac
    }
    ## snowInit cm water equivalent
    if ("SWE" %in% ic.names) {
      param[idx("snowInit"), 2] <- IC$SWE
    }
    ## microbeInit mgC/g soil
    if ("microbe" %in% ic.names) {
      param[idx("microbeInit"), 2] <- IC$microbe
    }

  } else if (length(settings$run$inputs$poolinitcond$path) > 0) {
    IC.path <- settings$run$inputs$poolinitcond$path
    if (length(IC.path) > 1) {
      PEcAn.logger::logger.error(
        "write.config.SIPNET needs one poolinitcond path",
        "got", length(IC.path)
      )
    }
    
    IC.pools <- PEcAn.data.land::prepare_pools(IC.path, constants = list(sla = SLA))
    
    if (!is.null(IC.pools)) {
      IC.nc <- ncdf4::nc_open(IC.path) #for additional variables specific to SIPNET

      # Optional variables: Use these if present, but don't complain if missing
      # TODO: Each variable here is used in a corresponding `if` block below,
      # which are mixed in among the variables from prepare_pools.
      # Should reorder to separate these, and consider making this an input
      # to let user control at runtime what's optional and what's mandatory
      ic_ncvars_to_try <- c(
        "nee",
        "SoilMoistFrac",
        "SWE",
        "date_of_budburst",
        "date_of_senescence",
        "Microbial Biomass C"
      )
      ic_has_ncvars <- ic_ncvars_to_try %in% names(IC.nc$var)
      names(ic_has_ncvars) <- ic_ncvars_to_try

      ## plantWoodInit gC/m2
      if ("wood" %in% names(IC.pools)) {
        fineRootFrac <- param[idx("fineRootFrac"),2]
        coarseRootFrac <- param[idx("coarseRootFrac"),2]
        # accounts for the fact that SIPNET take plantWoodInit as all woods (including roots).
        param[idx("plantWoodInit"), 2] <- PEcAn.utils::ud_convert(IC.pools$wood, "kg m-2", "g m-2")/(1-fineRootFrac-coarseRootFrac)
      }
      ## laiInit m2/m2
      lai <- IC.pools$LAI
      if (!is.na(lai) && is.numeric(lai)) {
        param[idx("laiInit"), 2] <- lai
      }

      # Sipnet always starts from initial LAI whether day 0 is in or out of the
      # growing season -> set LAI=0 when a deciduous PFT starts with leaves off
      #
      # Note: At this writing in Jan 2025, leafOnDay and LeafOffDay are taken
      # from the model defaults (template.param) unless:
      # - settings$run$inputs$leaf_phenology is provided, or
      # - the PFT sets leafOnDay/leafOffday as traits.
      # So unless you set something different, it's probably using DOY 144/285
      # ==> leaves are on from late May through mid-October.
      is_deciduous_pft <- isTRUE(param[idx("fracLeafFall"), 2] > 0.5)
      start_day <- lubridate::yday(settings$run$start.date)
      starts_with_leaves <- (
        start_day >= param[idx("leafOnDay"), 2]
        && start_day <= param[idx("leafOffDay"), 2]
      )
      if (is_deciduous_pft && !starts_with_leaves) {
        # Note that this doesn't adjust for winter LAI of evergreens!
        # Could consider using LAI*fracLeafFall,
        # But that strongly assumes that IC LAI is both (1) reported at
        # season peak and not (2) adjusted by any earlier step (i.e. SDA).
        param[idx("laiInit"), 2] <- 0
      }

      ## neeInit gC/m2
      if (ic_has_ncvars[["nee"]]) {
        nee <- ncdf4::ncvar_get(IC.nc, "nee")
        if (!is.na(nee) && is.numeric(nee)) {
          param[idx("neeInit"), 2] <- nee
        }
      }
      ## litterInit gC/m2
      if ("litter" %in% names(IC.pools)) {
        param[idx("litterInit"), 2] <- PEcAn.utils::ud_convert(IC.pools$litter, "g m-2", "g m-2") # BETY: kgC m-2
      }
      ## soilInit gC/m2
      if ("soil" %in% names(IC.pools)) {
        param[idx("soilInit"), 2] <- PEcAn.utils::ud_convert(sum(IC.pools$soil), "kg m-2", "g m-2") # BETY: kgC m-2
      }
      ## soilWFracInit fraction
      if (ic_has_ncvars[["SoilMoistFrac"]]) {
        soilWFrac <- ncdf4::ncvar_get(IC.nc, "SoilMoistFrac")
        if (!is.na(soilWFrac) && is.numeric(soilWFrac)) {
          param[idx("soilWFracInit"), 2] <- sum(soilWFrac) / 100
          ## litterWFracInit fraction
          litterWFrac <- soilWFrac
        }
      }
      
           
      ## snowInit cm water equivalent (cm = g / cm2 because 1 g water = 1 cm3 water)
      if (ic_has_ncvars[["SWE"]]) {
        snow <- ncdf4::ncvar_get(IC.nc, "SWE")
        if (!is.na(snow) && is.numeric(snow)) {
          param[idx("snowInit"), 2] <- PEcAn.utils::ud_convert(snow, "kg m-2", "g cm-2")  # BETY: kg m-2
        }
      }
      ## leafOnDay
      if (ic_has_ncvars[["date_of_budburst"]]) {
        leafOnDay <- ncdf4::ncvar_get(IC.nc, "date_of_budburst")
        if (!is.na(leafOnDay) && is.numeric(leafOnDay)) {
          param[idx("leafOnDay"), 2] <- leafOnDay
        }
      }
      ## leafOffDay
      if (ic_has_ncvars[["date_of_senescence"]]) {
        leafOffDay <- ncdf4::ncvar_get(IC.nc, "date_of_senescence")
        if (!is.na(leafOffDay) && is.numeric(leafOffDay)) {
          param[idx("leafOffDay"), 2] <- leafOffDay
        }
      }
      if (ic_has_ncvars[["Microbial Biomass C"]]) {
        microbe <- ncdf4::ncvar_get(IC.nc, "Microbial Biomass C")
        if (!is.na(microbe) && is.numeric(microbe)) {
          param[idx("microbeInit"), 2] <- PEcAn.utils::ud_convert(microbe, "mg kg-1", "mg g-1") #BETY: mg microbial C kg-1 soil
        }
      }

      ncdf4::nc_close(IC.nc)
    } else {
      PEcAn.logger::logger.error("Bad initial conditions filepath; keeping defaults")
    }
  } else {
    #some stuff about IC file that we can give in lieu of actual ICs
  }


  if (!is.null(settings$run$inputs$soilmoisture)) {
    #read soil moisture netcdf file, grab closet date to start_date, set equal to soilWFrac
    if (!is.null(settings$run$inputs$soilmoisture$path)) {
      soil.path <- settings$run$inputs$soilmoisture$path
      soilWFrac <- ncdf4::ncvar_get(ncdf4::nc_open(soil.path), varid = "mass_fraction_of_unfrozen_water_in_soil_moisture")

      param[idx("soilWFracInit"), 2] <- soilWFrac
    }

  }
  toc("apply_state_and_soil_inputs", t_state_inputs)
  if (file.exists(file.path(settings$rundir, run.id, "sipnet.param"))) {
    file.rename(
      file.path(settings$rundir, run.id, "sipnet.param"),
      file.path(
        settings$rundir,
        run.id,
        paste0("sipnet_", lubridate::year(settings$run$start.date), "_", lubridate::year(settings$run$end.date), ".param")
      )
    )
  }


  utils::write.table(
    param,
    file.path(settings$rundir, run.id, "sipnet.param"),
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  toc("write_config_total", t_main)
} # write.config.SIPNET







#--------------------------------------------------------------------------------------------------#
##'
##' Clear out previous SIPNET config and parameter files.
##'
##' @name remove.config.SIPNET
##' @title Clear out previous SIPNET config and parameter files.
##' @param main.outdir Primary PEcAn output directory (will be depreciated)
##' @param settings PEcAn settings file
##' @return nothing, removes config files as side effect
##' @export
##'
##' @author Shawn Serbin, David LeBauer
remove.config.SIPNET <- function(main.outdir, settings) {
  
  ### Remove files on localhost
  if (settings$host$name == "localhost") {
    files <- paste0(settings$outdir, list.files(path = settings$outdir, recursive = FALSE))  # Need to change this to the run folder when implemented
    files <- files[-grep("*.xml", files)]  # Keep pecan.xml file
    pft.dir <- strsplit(settings$pfts$pft$outdir, "/")[[1]]
    ln <- length(pft.dir)
    pft.dir <- pft.dir[ln]
    files <- files[-grep(pft.dir, files)]  # Keep pft folder
    # file.remove(files,recursive=TRUE)
    system(paste("rm -r ", files, sep = "", collapse = " "), ignore.stderr = TRUE)  # remove files/dirs
    
    ### On remote host
  } else {
    print("*** WARNING: Removal of files on remote host not yet implemented ***")
  }
} # remove.config.SIPNET 
