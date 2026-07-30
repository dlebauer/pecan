testthat::context("met2model")

testthat::test_that("Met conversion runs from the package fixture", {
  fixture_path <- ldndc_example_file("ldndc.met.2019.nc")
  outfolder <- withr::local_tempdir()

  testthat::expect_true(file.exists(fixture_path))

  result <- PEcAn.LDNDC::met2model.LDNDC(
    in.path = dirname(fixture_path),
    in.prefix = "ldndc.met",
    outfolder = outfolder,
    start_date = "2019-01-01",
    end_date = "2019-01-03"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(file.exists(result[["file"]][[1]]))
  testthat::expect_match(
    readLines(result[["file"]][[1]], n = 1),
    "%global",
    fixed = TRUE
  )
})
