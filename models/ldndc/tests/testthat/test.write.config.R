testthat::context("write.config")

testthat::test_that("write.config.LDNDC writes default airchemistry and runtime config launcher", {
  work_dir <- withr::local_tempdir(pattern = "ldndc-write-config-")
  run_root <- file.path(work_dir, "rundir")
  out_root <- file.path(work_dir, "outdir")
  dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  run_id <- "TEST-RUN"
  dir.create(file.path(run_root, run_id), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_root, run_id), recursive = TRUE, showWarnings = FALSE)

  fake_runtime_root <- file.path(work_dir, "runtime")
  dir.create(file.path(fake_runtime_root, "bin"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake_runtime_root, "dotfiles"), recursive = TRUE, showWarnings = FALSE)

  fake_binary <- file.path(fake_runtime_root, "bin", "ldndc")
  writeLines("#!/bin/sh\nexit 0\n", fake_binary)
  Sys.chmod(fake_binary, mode = "0755")

  events_path <- file.path(work_dir, "events.xml")
  writeLines("<ldndcevent></ldndcevent>", events_path)

  settings <- list(
    host = list(
      rundir = run_root,
      outdir = out_root
    ),
    rundir = run_root,
    model = list(
      binary = fake_binary,
      delete.raw = FALSE
    ),
    run = list(
      start.date = "2010-01-01",
      end.date = "2010-01-03",
      site = list(
        id = 1,
        lat = 46.56,
        lon = 11.03
      ),
      inputs = list(
        met = list(path = file.path(work_dir, "climate.txt")),
        events = list(path1 = events_path),
        airchemistry = list(path1 = NULL),
        poolinitcond = list(path = NULL),
        groundwater = list(path1 = NULL)
      )
    ),
    pfts = list(list(name = "meadow"))
  )

  trait_values <- list(meadow = list())

  PEcAn.LDNDC::write.config.LDNDC(
    defaults = NULL,
    trait.values = trait_values,
    settings = settings,
    run.id = run_id
  )

  airchemistry_path <- file.path(run_root, run_id, "airchemistry.txt")
  job_path <- file.path(run_root, run_id, "job.sh")
  project_path <- file.path(run_root, run_id, "project.ldndc")
  setup_path <- file.path(run_root, run_id, "setup.xml")
  site_path <- file.path(run_root, run_id, "site.xml")

  testthat::expect_true(file.exists(airchemistry_path))
  testthat::expect_true(file.exists(job_path))
  testthat::expect_true(file.exists(project_path))
  testthat::expect_true(file.exists(setup_path))
  testthat::expect_true(file.exists(site_path))
  testthat::expect_match(
    paste(readLines(airchemistry_path), collapse = "\n"),
    "%airchemistry",
    fixed = TRUE
  )

  job_lines <- readLines(job_path)
  normalized_binary <- normalizePath(fake_binary, winslash = "/", mustWork = FALSE)
  normalized_root <- dirname(dirname(normalized_binary))
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    paste0("LDNDC_BINARY=\"", normalized_binary, "\""),
    fixed = TRUE
  )
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    paste0("LDNDC_ROOT=\"", normalized_root, "\""),
    fixed = TRUE
  )
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    "resources_path = \"$LDNDC_ROOT\"",
    fixed = TRUE
  )
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    "udunits_path = \"$LDNDC_ROOT/dotfiles\"",
    fixed = TRUE
  )
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    "\"$LDNDC_BINARY\" -c \"$LDNDC_CONFIG\" \"$LDNDC_PROJECT\"",
    fixed = TRUE
  )
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    "NEEDS_RUN=0",
    fixed = TRUE
  )
  testthat::expect_no_match(paste(job_lines, collapse = "\n"), "@DELETE_RAW@", fixed = TRUE)
  testthat::expect_match(
    paste(job_lines, collapse = "\n"),
    "model2netcdf.LDNDC",
    fixed = TRUE
  )

  project_lines <- paste(readLines(project_path), collapse = "\n")
  setup_lines <- paste(readLines(setup_path), collapse = "\n")
  site_lines <- paste(readLines(site_path), collapse = "\n")

  testthat::expect_no_match(project_lines, "@Groundwater@", perl = TRUE)
  testthat::expect_no_match(project_lines, "''", fixed = TRUE)
  testthat::expect_match(
    setup_lines,
    "<module id='output:report:arable' timemode='subdaily' />",
    fixed = TRUE
  )
  testthat::expect_no_match(setup_lines, "'<module", fixed = TRUE)
  testthat::expect_match(
    site_lines,
    "<general usehistory='arable' soil='SALO' lheight='0.0' />",
    fixed = TRUE
  )
  testthat::expect_no_match(site_lines, "@Info_Use_History@", perl = TRUE)
})

testthat::test_that("write.config.LDNDC can use settings-provided support files", {
  work_dir <- withr::local_tempdir(pattern = "ldndc-write-config-support-")
  run_root <- file.path(work_dir, "rundir")
  out_root <- file.path(work_dir, "outdir")
  dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  run_id <- "SUPPORT-RUN"
  dir.create(file.path(run_root, run_id), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_root, run_id), recursive = TRUE, showWarnings = FALSE)

  fake_runtime_root <- file.path(work_dir, "runtime")
  dir.create(file.path(fake_runtime_root, "bin"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake_runtime_root, "dotfiles"), recursive = TRUE, showWarnings = FALSE)

  fake_binary <- file.path(fake_runtime_root, "bin", "ldndc")
  writeLines("#!/bin/sh\nexit 0\n", fake_binary)
  Sys.chmod(fake_binary, mode = "0755")

  settings <- list(
    host = list(
      rundir = run_root,
      outdir = out_root
    ),
    rundir = run_root,
    model = list(
      binary = fake_binary,
      delete.raw = FALSE
    ),
    run = list(
      start.date = "2019-01-01",
      end.date = "2019-12-31",
      site = list(
        id = 1,
        lat = 46.56,
        lon = 11.03
      ),
      inputs = list(
        met = list(path = file.path(work_dir, "climate.txt")),
        events = list(path1 = ldndc_example_file("events.xml")),
        setup = list(path1 = ldndc_example_file("setup.xml")),
        site = list(path1 = ldndc_example_file("site.xml")),
        siteparameters = list(path1 = ldndc_example_file("siteparameters.xml")),
        speciesparameters = list(path1 = ldndc_example_file("speciesparameters.xml")),
        airchemistry = list(path1 = ldndc_example_file("airchemistry.txt"))
      )
    ),
    pfts = list(list(name = "meadow"))
  )

  PEcAn.LDNDC::write.config.LDNDC(
    defaults = NULL,
    trait.values = readRDS(ldndc_example_file("trait_values.rds")),
    settings = settings,
    run.id = run_id
  )

  run_dir <- file.path(run_root, run_id)
  job_lines <- paste(readLines(file.path(run_dir, "job.sh")), collapse = "\n")
  site_lines <- paste(readLines(file.path(run_dir, "site.xml")), collapse = "\n")
  species_lines <- paste(readLines(file.path(run_dir, "speciesparameters.xml")), collapse = "\n")

  testthat::expect_match(site_lines, "usehistory=\"grassland\"", fixed = TRUE)
  testthat::expect_match(species_lines, "mnemonic=\"PERG\"", fixed = TRUE)
  testthat::expect_no_match(job_lines, "@DELETE_RAW@", fixed = TRUE)
  testthat::expect_match(job_lines, "FALSE", fixed = TRUE)
})
