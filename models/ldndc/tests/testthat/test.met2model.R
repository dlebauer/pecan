testthat::context("met2model")

write_ldndc_met_fixture <- function(target_dir) {
  source_file <- testthat::test_path(
    "..", "..", "..", "basgra", "tests", "testthat", "test.met.2019.nc"
  )
  target_file <- file.path(target_dir, "ldndc.met.2019.nc")
  file.copy(source_file, target_file, overwrite = TRUE)

  nc <- ncdf4::nc_open(target_file, write = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  air_temperature <- ncdf4::ncvar_get(nc, "air_temperature")
  dims <- nc[["var"]][["air_temperature"]][["dim"]]

  nc <- ncdf4::ncvar_add(
    nc,
    ncdf4::ncvar_def("air_pressure", "Pa", dim = dims, missval = -9999)
  )
  nc <- ncdf4::ncvar_add(
    nc,
    ncdf4::ncvar_def(
      "water_vapor_saturation_deficit",
      "Pa",
      dim = dims,
      missval = -9999
    )
  )

  ncdf4::ncvar_put(
    nc,
    "air_pressure",
    array(101325, dim = dim(air_temperature))
  )
  ncdf4::ncvar_put(
    nc,
    "water_vapor_saturation_deficit",
    array(500, dim = dim(air_temperature))
  )

  target_file
}

testthat::test_that("Met conversion runs without error", {
  in_path <- withr::local_tempdir()
  outfolder <- withr::local_tempdir()

  fixture_path <- write_ldndc_met_fixture(in_path)

  testthat::expect_true(file.exists(fixture_path))

  result <- PEcAn.LDNDC::met2model.LDNDC(
    in.path = in_path,
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
