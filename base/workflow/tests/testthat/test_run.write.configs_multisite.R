test_that("run.write.configs merges multi-site SA runs without clobbering samples.Rdata", {
  # Stub model hooks so we don't need a real model package
  assign("write.config.FAKE", function(defaults, trait.values, settings, run.id) {
    dir.create(file.path(settings$rundir, run.id), recursive = TRUE, showWarnings = FALSE)
    # Record which met path was chosen for this run
    writeLines(as.character(settings$run$inputs$met$path), file.path(settings$rundir, run.id, "config.txt"))
  }, envir = .GlobalEnv)
  assign("remove.config.FAKE", function(...) invisible(TRUE), envir = .GlobalEnv)
  withr::defer(rm(list = c("write.config.FAKE", "remove.config.FAKE"), envir = .GlobalEnv))

  workflow_root <- withr::local_tempdir()
  rundirs <- list(
    A = file.path(workflow_root, "rundir_A"),
    B = file.path(workflow_root, "rundir_B")
  )
  lapply(rundirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  met_paths <- as.list(file.path(workflow_root, "met", paste0("met", 1:3, ".nc")))
  dir.create(file.path(workflow_root, "met"), showWarnings = FALSE, recursive = TRUE)
  lapply(met_paths, function(p) writeLines("dummy", p))

  samples_file <- file.path(workflow_root, "samples.Rdata")
  trait.samples <- list(
    deciduous = list(Vcmax = 1:4, SLA = 5:8),
    conifer = list(Vcmax = 11:14, SLA = 15:18)
  )
  sa.samples <- list(
    deciduous = matrix(c(1, 2, 3, 4),
      nrow = 2, byrow = TRUE,
      dimnames = list(c("50", "95"), c("Vcmax", "SLA"))
    ),
    conifer = matrix(c(5, 6, 7, 8),
      nrow = 2, byrow = TRUE,
      dimnames = list(c("50", "95"), c("Vcmax", "SLA"))
    )
  )
  runs.samples <- list()
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  save(trait.samples, sa.samples, runs.samples, pft.names, trait.names, file = samples_file)

  input_design <- data.frame(param = 1:3, met = c(1, 2, 3))

  make_settings <- function(site_name, site_id, site_pft, rundir) {
    list(
      outdir = workflow_root,
      rundir = rundir,
      modeloutdir = rundir,
      database = NULL,
      host = list(name = "localhost", rundir = rundir, outdir = workflow_root),
      model = list(type = "FAKE", id = 123),
      run = list(
        start.date = "2001/01/01",
        end.date = "2001/12/31",
        outdir = workflow_root,
        site = list(id = site_id, name = site_name, lat = 40, lon = -88, site.pft = list(site_pft)),
        inputs = list(met = list(path = met_paths))
      ),
      pfts = list(
        list(name = "deciduous", constants = list(SLA = 2), posteriorid = NULL),
        list(name = "conifer", constants = list(SLA = 3), posteriorid = NULL)
      ),
      sensitivity.analysis = list(quantiles = c(0.05, 0.5, 0.95)),
      workflow = list(id = 321)
    )
  }

  settings_A <- make_settings("Site-A", "100000001", "deciduous", rundirs$A)
  settings_B <- make_settings("Site-B", "100000002", "conifer", rundirs$B)

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg", function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.sa.configs", function(defaults, quantile.samples, settings, model, input_design = NULL, write.to.db = TRUE, ...) {
    site_pfts <- unique(unlist(settings$run$site$site.pft))
    runs <- list()
    run_lines <- character(0)
    for (pft in site_pfts) {
      samples <- quantile.samples[[pft]]
      if (is.null(samples)) {
        next
      }
      traits <- colnames(samples)
      quantiles <- rownames(samples)
      run_ids <- matrix(
        "",
        nrow = length(quantiles),
        ncol = length(traits),
        dimnames = list(quantiles, traits)
      )
      median_label <- "50"
      median_id <- paste0("SA-median-", settings$run$site$id)
      run_ids[median_label, ] <- median_id
      local_runs <- median_id
      for (q in setdiff(quantiles, median_label)) {
        for (trait in traits) {
          current_id <- paste0("SA-", settings$run$site$id, "-", pft, "-", trait, "-", q)
          run_ids[q, trait] <- current_id
          local_runs <- c(local_runs, current_id)
        }
      }
      runs[[pft]] <- run_ids
      run_lines <- c(run_lines, local_runs)
      settings_copy <- settings
      met_path <- settings_copy$run$inputs$met$path
      if (is.list(met_path)) {
        settings_copy$run$inputs$met$path <- met_path[[1]]
      }
      for (run_id in local_runs) {
        write.config.FAKE(defaults, samples, settings_copy, run_id)
      }
    }
    if (length(run_lines) > 0) {
      cat(run_lines, file = file.path(settings$rundir, "runs.txt"), sep = "\n")
    }
    list(runs = runs, ensemble.id = paste0("E-", settings$run$site$id))
  })

  run_write_configs(
    settings = settings_A,
    ensemble.size = nrow(input_design),
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  first_env <- new.env()
  load(samples_file, envir = first_env)
  expect_true(all(nchar(as.matrix(first_env$runs.samples$sa$deciduous)) > 0))
  expect_null(first_env$runs.samples$sa$conifer)

  run_write_configs(
    settings = settings_B,
    ensemble.size = nrow(input_design),
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  merged <- new.env()
  load(samples_file, envir = merged)
  expect_true(all(nchar(as.matrix(merged$runs.samples$sa$deciduous)) > 0))
  expect_true(all(nchar(as.matrix(merged$runs.samples$sa$conifer)) > 0))

  runs_A <- readLines(file.path(rundirs$A, "runs.txt"))
  runs_B <- readLines(file.path(rundirs$B, "runs.txt"))
  expect_equal(length(runs_A), 3)
  expect_equal(length(runs_B), 3)
  expect_true(file.exists(file.path(rundirs$A, runs_A[2], "config.txt")))
  expect_true(file.exists(file.path(rundirs$B, runs_B[2], "config.txt")))
})
