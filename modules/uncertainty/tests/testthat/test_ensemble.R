library(testthat)
library(mockery)

# Source the ensemble config function
source("modules/uncertainty/R/ensemble.R")

context("input validation for write.ensemble.configs")

# Mock a model write.configs function to avoid model-specific errors
write.configs.SIPNET <- function(...) TRUE

# Helper: make input with correct structure
make_input_sets <- function(paths) {
  lapply(paths, function(p) list(path = p))
}

# Helper: make ensemble.samples with the correct structure
make_samples <- function(samples) {
  list(input = data.frame(samples = samples, stringsAsFactors = FALSE))
}

# 1. One input, no samples → should pass
test_that("1 input, no samples: passes", {
  settings <- list(run = list(inputs = list(input = list(path = "IC1"))))
  ensemble.samples <- NULL
  defaults <- list()
  
  expect_silent(write.ensemble.configs(
    defaults = defaults,
    ensemble.samples = ensemble.samples,
    settings = settings,
    model = "SIPNET",
    write.to.db = FALSE
  ))
})



test_that("no input error", {
  settings <- list(run = list(inputs = list(input = NULL)))
  ensemble.samples <- NULL
  defaults <- list()
  
  # Capture logger message
  expect_silent(write.ensemble.configs(
    defaults = defaults,
    ensemble.samples = ensemble.samples,
    settings = settings,
    model = "SIPNET",
    write.to.db = FALSE
  ))
})



test_that("multiple inputs, valid matching samples ", {
  settings <- list(run = list(inputs = list(input = NULL)))  # or missing/empty paths
  ensemble.samples <- make_samples(c("IC1", "IC2", "IC3", "IC2"))
  defaults <- list()
  
  # Capture logger message, but don't stop execution
  expect_silent(write.ensemble.configs(
    defaults = defaults,
    ensemble.samples = ensemble.samples,
    settings = settings,
    model = "SIPNET",
    write.to.db = FALSE
  ))
})


