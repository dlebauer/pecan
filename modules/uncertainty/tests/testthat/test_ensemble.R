library(testthat)
library(PEcAn.logger)
library(PEcAn.DB)


source("modules/uncertainty/R/ensemble.R")
dummy_binary_path <- file.path(tempdir(), "sipnet")
file.create(dummy_binary_path)
# Mock SIPNET writer
if (!exists("write.config.SIPNET")) {
  write.config.SIPNET <- function(...) {
    PEcAn.logger::logger.info("Mock SIPNET writer called")
    return(invisible(TRUE))
  }
}

context("Ensemble Input Validation Tests")

create_base_settings <- function() {
  list(
    workflow = list(id = 1),
    model = list(
      id = 1000,
      type = "SIPNET",
      binary = dummy_binary_path
    ),
    run = list(
      site = list(id = 1, name = "Test Site", lat = 40.0, lon = -80.0),
      start.date = "2004-01-01",
      end.date = "2004-12-31"
    ),
    host = list(
      outdir = tempdir(),
      rundir = tempdir(),
      name = "localhost"
    ),
    database = list(bety = list(write = FALSE))
  )
}

test_that("Single input with no samples works", {
  withr::local_tempdir()
  
  def <- list(
    inputs = list(soil = list(path = "soil1.nc")),
    pfts = list(list(
      name = "temperate.pft",
      constants = list(param1 = 0.5)
    )),
    model = list(id = 1000),
    database = list(bety = list(write = FALSE))
  )
  
  settings <- create_base_settings()
  settings$run$inputs <- list(soil = list(path = "soil1.nc"))
  settings$ensemble <- list(size = 1)
  
  writeLines("", "soil1.nc")
  
  result <- write.ensemble.configs(def, NULL, settings, "SIPNET")
  expect_true(!is.null(result$runs))
  expect_true(!is.null(result$ensemble.id))
})

test_that("Multiple inputs without samples throws error", {
  def <- list(
    inputs = list(soil = list(path = c("soil1.nc", "soil2.nc"))),
    pfts = list(list(
      name = "temperate.pft",
      constants = list(param1 = 0.5)
    )),
    model = list(id = 1000),
    database = list(bety = list(write = FALSE))
  )
  
  settings <- create_base_settings()
  settings$ensemble <- list(size = 1)
  
  purrr::walk(c("soil1.nc", "soil2.nc"), ~ writeLines("", .x))
  
  expect_error(
    write.ensemble.configs(def, NULL, settings, "SIPNET"),
    "Multiple soil inputs found but no sampling method specified"
  )
})
