test_that("run.write.configs coordinates SIPNET multi-site SA inputs", {
    old_level <- PEcAn.logger::logger.setLevel("ERROR")
    withr::defer(PEcAn.logger::logger.setLevel(old_level), priority = "last")

    testthat::skip_if_not_installed("PEcAn.workflow")
    testthat::skip_if_not_installed("PEcAn.uncertainty")

    stub_env <- new.env(parent = emptyenv())
    stub_env$remove.config.SIPNET <- function(...) invisible(TRUE)
    attach(stub_env, name = "pecan_sipnet_stub")
    withr::defer(detach("pecan_sipnet_stub"), priority = "first")

    root <- withr::local_tempdir()
    workflow_root <- file.path(root, "workflow")
    dir.create(workflow_root, recursive = TRUE, showWarnings = FALSE)

    dirs <- list(
        run_siteA = file.path(workflow_root, "run_siteA"),
        run_siteB = file.path(workflow_root, "run_siteB"),
        model_siteA = file.path(workflow_root, "model_siteA"),
        model_siteB = file.path(workflow_root, "model_siteB")
    )
    lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

    met_template <- system.file("niwot.clim", package = "PEcAn.SIPNET")
    met_dir <- file.path(workflow_root, "met")
    dir.create(met_dir, recursive = TRUE, showWarnings = FALSE)
    met_paths <- file.path(met_dir, c("met_option1.clim", "met_option2.clim"))
    file.copy(met_template, met_paths)

    samples_file <- file.path(workflow_root, "samples.Rdata")
    trait.samples <- list(
        deciduous = list(
            Vcmax = seq(40, 55, length.out = 4),
            SLA = seq(10, 13, length.out = 4)
        ),
        conifer = list(
            Vcmax = seq(30, 45, length.out = 4),
            SLA = seq(6, 9, length.out = 4)
        )
    )
    quantiles <- c("05", "50", "95")
    sa.samples <- list(
        deciduous = matrix(
            c(
                38, 9,
                55, 11,
                70, 13
            ),
            nrow = length(quantiles), byrow = TRUE,
            dimnames = list(quantiles, c("Vcmax", "SLA"))
        ),
        conifer = matrix(
            c(
                28, 5,
                44, 7,
                60, 8
            ),
            nrow = length(quantiles), byrow = TRUE,
            dimnames = list(quantiles, c("Vcmax", "SLA"))
        )
    )
    runs.samples <- list()
    pft.names <- names(trait.samples)
    trait.names <- lapply(trait.samples, names)

    save(trait.samples, sa.samples, runs.samples, pft.names, trait.names,
        file = samples_file
    )

    input_design <- data.frame(
        param = 1:4,
        met = c(1, 2, 2, 1)
    )

    make_settings <- function(site_name, site_id, site_pft, rundir, modelout) {
        list(
            outdir = workflow_root,
            rundir = rundir,
            modeloutdir = modelout,
            database = NULL,
            host = list(
                name = "localhost",
                rundir = rundir,
                outdir = workflow_root
            ),
            model = list(type = "SIPNET", id = 99, binary = "sipnet", revision = "test"),
            run = list(
                start.date = "2001/01/01",
                end.date = "2001/12/31",
                outdir = workflow_root,
                site = list(id = site_id, name = site_name, lat = 40, lon = -88, site.pft = list(site_pft)),
                inputs = list(
                    met = list(path = as.list(met_paths))
                )
            ),
            pfts = list(
                list(name = "deciduous", constants = list(SLA = 2), posteriorid = NULL),
                list(name = "conifer", constants = list(SLA = 3), posteriorid = NULL)
            ),
            sensitivity.analysis = list(quantiles = c(0.05, 0.5, 0.95)),
            workflow = list(id = 321)
        )
    }

    settings_siteA <- make_settings("Site-A", "100000001", "deciduous", dirs$run_siteA, dirs$model_siteA)
    settings_siteB <- make_settings("Site-B", "100000002", "conifer", dirs$run_siteB, dirs$model_siteB)

    PEcAn.workflow::run.write.configs(
        settings = settings_siteA,
        ensemble.size = nrow(input_design),
        input_design = input_design,
        write = FALSE,
        overwrite = TRUE
    )

    met_lines <- function(rundir) {
        runs_file <- file.path(rundir, "runs.txt")
        run_ids <- readLines(runs_file)
        sa_runs <- run_ids[grepl("^SA-", run_ids) & !grepl("-median-", run_ids)]
        expect_true(length(sa_runs) >= nrow(input_design))
        sa_runs <- sa_runs[seq_len(nrow(input_design))]
        vapply(sa_runs, function(run_id) {
            readme_path <- file.path(rundir, run_id, "README.txt")
            readme <- readLines(readme_path)
            met_line <- trimws(grep("^met", readme, value = TRUE))
            met_line
        }, character(1))
    }

    samples_env <- new.env()
    load(samples_file, envir = samples_env)
    dec_ids <- as.matrix(samples_env$runs.samples$sa$deciduous)
    expect_true(all(nchar(dec_ids) > 0))

    PEcAn.workflow::run.write.configs(
        settings = settings_siteB,
        ensemble.size = nrow(input_design),
        input_design = input_design,
        write = FALSE,
        overwrite = TRUE
    )

    samples_env_b <- new.env()
    load(samples_file, envir = samples_env_b)
    conif_ids <- as.matrix(samples_env_b$runs.samples$sa$conifer)
    expect_true(all(nchar(conif_ids) > 0))

    met_a <- met_lines(settings_siteA$rundir)
    met_b <- met_lines(settings_siteB$rundir)
    expect_equal(length(met_a), nrow(input_design))
    expect_equal(length(met_b), nrow(input_design))
    expect_true(all(nchar(met_a) > 0))
    expect_true(all(nchar(met_b) > 0))
})
