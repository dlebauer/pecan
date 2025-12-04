test_that("run.write.configs merges multi-site SA runs without clobbering samples.Rdata", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  workflow_root <- withr::local_tempdir()
  samples_file <- file.path(workflow_root, "samples.Rdata")
  rundirs <- list(
    deciduous = file.path(workflow_root, "rundir_dec"),
    conifer = file.path(workflow_root, "rundir_con")
  )
  lapply(rundirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  trait.samples <- list(
    deciduous = list(Vcmax = 1:4),
    conifer = list(Vcmax = 11:14)
  )
  sa.samples <- list(
    deciduous = matrix(
      c(1, 2),
      nrow = 2,
      ncol = 1,
      dimnames = list(c("50", "95"), "Vcmax")
    ),
    conifer = matrix(
      c(3, 4),
      nrow = 2,
      ncol = 1,
      dimnames = list(c("50", "95"), "Vcmax")
    )
  )
  runs.samples <- list()
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  save(trait.samples, sa.samples, runs.samples, pft.names, trait.names, file = samples_file)

  input_design <- data.frame(param = c(1, 2), met = c(1, 1))

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
        inputs = list(met = list(path = list("met1.nc")))
      ),
      pfts = list(
        list(name = "deciduous", constants = list(SLA = 2), posteriorid = NULL),
        list(name = "conifer", constants = list(SLA = 3), posteriorid = NULL)
      ),
      sensitivity.analysis = list(quantiles = c(0.5, 0.95)),
      workflow = list(id = 42)
    )
  }

  settings_dec <- make_settings("Deciduous", "100001", "deciduous", rundirs$deciduous)
  settings_con <- make_settings("Conifer", "100002", "conifer", rundirs$conifer)

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg", function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.sa.configs", function(defaults, quantile.samples, settings, model, input_design = NULL, write.to.db = TRUE, ...) {
    site_pfts <- unique(unlist(settings$run$site$site.pft))
    runs <- list()
    for (pft in site_pfts) {
      samples <- quantile.samples[[pft]]
      traits <- colnames(samples)
      quantiles <- rownames(samples)
      run_ids <- matrix(
        "",
        nrow = length(quantiles),
        ncol = length(traits),
        dimnames = list(quantiles, traits)
      )
      for (trait in traits) {
        for (q in quantiles) {
          run_ids[q, trait] <- paste("SA", settings$run$site$id, pft, trait, q, sep = "-")
        }
      }
      runs[[pft]] <- run_ids
    }
    list(runs = runs, ensemble.id = paste0("E-", settings$run$site$id))
  })

  run_write_configs(
    settings = settings_dec,
    ensemble.size = nrow(input_design),
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  first_env <- new.env()
  load(samples_file, envir = first_env)
  first_dec_runs <- first_env$runs.samples$sa$deciduous
  expect_true(all(nchar(first_dec_runs) > 0))
  expect_null(first_env$runs.samples$sa$conifer)

  run_write_configs(
    settings = settings_con,
    ensemble.size = nrow(input_design),
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  merged <- new.env()
  load(samples_file, envir = merged)
  expect_equal(merged$runs.samples$sa$deciduous, first_dec_runs)
  expect_true(all(nchar(as.matrix(merged$runs.samples$sa$conifer)) > 0))
})
