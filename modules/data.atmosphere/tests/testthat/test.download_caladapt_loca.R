# Create a valid polygon as an sf object (simple square)
polygon_sf <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(
        c(-120, 35.8),
        c(-119.9, 35.8),
        c(-119.9, 36),
        c(-120, 36),
        c(-120, 35.8)
    ))),
    crs = 4326
))

# Create a valid point as an sf object
point_sf <- data.frame(lon = -120, lat = 36) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bad_point_sf <- data.frame(lon = -180, lat = 0) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

test_that("download_caladapt_loca_raster returns a tibble with expected columns for polygon input", {
    skip_on_cran()
    skip_on_ci()
    tmp_dir <- withr::local_tempdir()
    result <- download_caladapt_loca_raster(
        sf_obj = polygon_sf,
        var = "pr",
        gcm = "HadGEM2-ES",
        scenario = "historical",
        period = "year",
        start_year = 2001,
        end_year = 2003,
        out_dir = tmp_dir
    )
    if (is.na(result$raster[1])) {
        skip("Gateway Timeout encountered; skipping test")
    }
    expect_s3_class(result, "tbl_df")
    expect_in(
        c("var", "gcm", "scenario", "period", "start_year", "end_year", "raster"),
        colnames(result)
    )
    expect_true(file.exists(result$raster))
})

test_that("download_caladapt_loca_raster returns a tibble with expected columns for point input", {
    skip_on_cran()
    skip_on_ci()
    tmp_dir <- withr::local_tempdir()
    result <- download_caladapt_loca_raster(
        sf_obj = point_sf,
        var = "tasmax",
        gcm = "HadGEM2-ES",
        scenario = "historical",
        period = "year",
        start_year = 2001,
        end_year = 2003,
        out_dir = tmp_dir
    )
    if (is.na(result$raster[1])) {
        skip("Gateway Timeout encountered; skipping test")
    }
    expect_s3_class(result, "tbl_df")
    expect_in(
        c("var", "gcm", "scenario", "period", "start_year", "end_year", "raster"),
        colnames(result)
    )
    expect_true(file.exists(result$raster))
})
## TODO: test that it takes terra SpatVector objects