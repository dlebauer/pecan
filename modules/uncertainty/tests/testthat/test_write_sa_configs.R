# Test that write.sa.configs
# - uses input_design to choose inputs for each run
# - run IDs are recorded in the runs matrix
# - runs.txt exists, has one line per run, runs have corresponding directories
# Uses mock function write.config.FAKE

test_that("write.sa.configs coordinates input_design", {
  #---- Setup ----#
  # Setup test fixtures:
  #   directories, settings, input_design, write.config.FAKE, etc
  rundir <- withr::local_tempdir(pattern = "sa-rundir-")
  modeloutdir <- withr::local_tempdir(pattern = "sa-modelout-")

  met_paths <- c("met_path_1", "met_path_2", "met_path_3")
  settings <- list(
    rundir = rundir,
    modeloutdir = modeloutdir,
    host = list(name = "localhost", rundir = rundir, outdir = modeloutdir),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(id = "1", name = "Test Site", site.pft = list("pftA")),
      inputs = list(met = list(path = met_paths)),
      outdir = modeloutdir
    ),
    model = list(id = 99, type = "FAKE"),
    pfts = list(list(name = "pftA", posteriorid = 10, constants = list())),
    sensitivity.analysis = list(ensemble.id = "E-TEST"),
    workflow = list(id = 42),
    database = NULL
  )

  quantile.samples <- list(
    pftA = matrix(
      c(
        1, 2,
        3, 4
      ),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("50", "95"), c("trait1", "trait2"))
    )
  )

  input_design <- data.frame(
    param = 1:3,
    met = rep(1, 3)
  )

  assign("write.config.FAKE", function(defaults, trait.values, settings, run.id) {
    path_file <- file.path(settings$rundir, run.id, "met_path.txt")
    writeLines(settings$run$inputs$met$path, path_file)
    invisible(NULL)
  }, envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv))

  result <- PEcAn.uncertainty::write.sa.configs(
    defaults = settings$pfts,
    quantile.samples = quantile.samples,
    settings = settings,
    model = "FAKE",
    write.to.db = FALSE,
    input_design = input_design
  )
  #---- Tests ----#
  # Test that result has expected structure and content
  expect_equal(result$ensemble.id, "E-TEST")
  median_id <- "SA-median-1"
  trait1_id <- "SA-pftA-trait1-0.95-1"
  trait2_id <- "SA-pftA-trait2-0.95-1"

  expect_equal(
    as.character(result$runs$pftA["50", c("trait1", "trait2")]),
    rep(median_id, 2)
  )
  expect_equal(as.character(result$runs$pftA["95", "trait1"]), trait1_id)
  expect_equal(as.character(result$runs$pftA["95", "trait2"]), trait2_id)
  # Test that runs directories were created and contain expected met paths
  runs_file <- file.path(rundir, "runs.txt")
  expect_true(file.exists(runs_file))
  expect_length(readLines(runs_file), 3)

  # Check that each run directory has the correct met path
  expect_true(readLines(file.path(rundir, trait1_id, "met_path.txt")) %in% met_paths)
  expect_true(readLines(file.path(rundir, trait2_id, "met_path.txt")) %in% met_paths)
})
