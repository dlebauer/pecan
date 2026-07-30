testthat::context("model2netcdf")

testthat::test_that("NetCDF conversion reads current soil chemistry output", {
  outdir <- withr::local_tempdir(pattern = "ldndc-output-")
  dir.create(file.path(outdir, "Output"), recursive = TRUE, showWarnings = FALSE)

  datetimes <- c(
    "2000-01-01 00:00:00",
    "2000-01-01 06:00:00",
    "2000-01-01 12:00:00",
    "2000-01-01 18:00:00",
    "2000-01-02 00:00:00"
  )

  readr::write_tsv(
    data.frame(
      datetime = datetimes,
      species = "all",
      lai = seq(1, 1.4, length.out = length(datetimes)),
      dC_co2_upt.kgCm.2. = rep(0.1, length(datetimes)),
      dC_maintenance_resp.kgCm.2. = rep(0.02, length(datetimes)),
      dC_transport_resp.kgCm.2. = rep(0.01, length(datetimes)),
      dC_growth_resp.kgCm.2. = rep(0.01, length(datetimes)),
      DW_below.kgDWm.2. = rep(0.2, length(datetimes)),
      DW_above.kgDWm.2. = rep(0.3, length(datetimes))
    ),
    file.path(outdir, "Output", "physiology-subdaily.txt")
  )

  readr::write_tsv(
    data.frame(
      datetime = datetimes,
      sC_co2_prod_hetero.kgCm.2. = rep(0.03, length(datetimes))
    ),
    file.path(outdir, "Output", "soilchemistry-subdaily.txt")
  )

  readr::write_tsv(
    data.frame(
      datetime = datetimes,
      soilwater_10cm... = rep(0.2, length(datetimes)),
      soilwater_30cm... = rep(0.25, length(datetimes))
    ),
    file.path(outdir, "Output", "watercycle-subdaily.txt")
  )

  readr::write_tsv(
    data.frame(
      datetime = "2000-01-01 00:00:00",
      dC_fru_export.kgCha.1. = 0,
      dC_fol_export.kgCha.1. = 0,
      dC_frt_export.kgCha.1. = 0,
      dC_lst_above_export.kgCha.1. = 0,
      dC_lst_below_export.kgCha.1. = 0,
      dC_dst_above_export.kgCha.1. = 0,
      dC_dst_below_export.kgCha.1. = 0,
      dC_straw_export.kgCha.1. = 0
    ),
    file.path(outdir, "Output", "report-harvest.txt")
  )

  readr::write_tsv(
    data.frame(
      datetime = "2000-01-01 00:00:00",
      dC_fru_export.kgCha.1. = 0,
      dC_fol_export.kgCha.1. = 0,
      dC_dfol_export.kgCha.1. = 0,
      dC_lst_export.kgCha.1. = 0,
      dC_dst_export.kgCha.1. = 0,
      dC_frt_export.kgCha.1. = 0
    ),
    file.path(outdir, "Output", "report-cut.txt")
  )

  PEcAn.LDNDC::model2netcdf.LDNDC(
    outdir,
    60,
    25,
    "2000-01-01",
    "2000-01-01",
    delete.raw = FALSE
  )

  testthat::expect_true(file.exists(file.path(outdir, "2000.nc")))
})
