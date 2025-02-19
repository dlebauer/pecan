# Test setup: temporary datasets and files
withr::with_tempdir({
  temp_ensemble_data_rds <- "ensemble_data.rds"
  temp_coords_csv <- "final_design_points.csv"
  file.remove(temp_ensemble_data_rds, temp_coords_csv)

  set.seed(123)
  ensemble_data <- list(
    "2020-01-01" = data.frame(
      site_id = 1:10,
      SOC = runif(10, 8, 15),
      AGB = runif(10, -0.5, 0.5)
    )
  )
  saveRDS(ensemble_data, temp_ensemble_data)

  # Generate test coordinates with 10 values
  site_coordinates <- data.frame(
    id = 1:10,
    lat = runif(10, 33.5, 34.5),
    lon = runif(10, -118, -117)
  )
  write.csv(site_coordinates, temp_coords_csv, row.names = FALSE)

  # Test `SDA_downscale_preprocess`
  test_that("SDA_downscale_preprocess handles both file and objects as inputs", {
    # file inputs
    processed_data <- SDA_downscale_preprocess(
      ensemble_data = temp_ensemble_data_rds,
      date = "2020-01-01",
      carbon_pool = "SOC",
      site_coords = site_coordinates
    )
    # object inputs
    processed_data2 <- SDA_downscale_preprocess(
      ensemble_data = ensemble_data,
      date = "2020-01-01",
      carbon_pool = "SOC",
      site_coords = temp_coords_csv
    )
    expect_identical(processed_data, processed_data2)

    expect_true(is.list(processed_data))
    expect_true("input_data" %in% names(processed_data))
    expect_true("site_coordinates" %in% names(processed_data))
    expect_true("carbon_data" %in% names(processed_data))
    expect_true(is.data.frame(processed_data$site_coordinates))
    expect_true("id" %in% colnames(processed_data$site_coordinates))
    expect_true("lat" %in% colnames(processed_data$site_coordinates))
    expect_true("lon" %in% colnames(processed_data$site_coordinates))
    expect_true(is.data.frame(processed_data$carbon_data))
    expect_true("ensemble1" %in% colnames(processed_data$carbon_data))
  })

  # Generate test raster data
  r <- terra::rast(ncols = 10, nrows = 10)
  values(r) <- runif(100)

  # Create preprocessed data object with 10 values
  preprocessed <- list(
    input_data = ensemble_data,
    site_coordinates = site_coordinates,
    carbon_data = data.frame(
      ensemble1 = runif(10, 8, 15)
    )
  )

  # Test `SDA_downscale`
  test_that("SDA_downscale works with sf coordinates and raster covariates", {
    downscaled_results <- SDA_downscale(
      preprocessed = preprocessed,
      carbon_pool = "SOC",
      covariates = r,
      model_type = "rf"
    )

    expect_true(is.list(downscaled_results))
    expect_true("data" %in% names(downscaled_results))
    expect_true("models" %in% names(downscaled_results))
    expect_true("maps" %in% names(downscaled_results))
  })
})
