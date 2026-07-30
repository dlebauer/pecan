ldndc_example_file <- function(...) {
  installed <- system.file("extdata", "DE_graswang", ..., package = "PEcAn.LDNDC")
  if (nzchar(installed)) {
    return(installed)
  }

  testthat::test_path("..", "..", "inst", "extdata", "DE_graswang", ...)
}
