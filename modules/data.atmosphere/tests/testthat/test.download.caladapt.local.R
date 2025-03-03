# Create a valid polygon as an sf object (simple square)
polygon_sf <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(
        c(-120, 35),
        c(-119, 35),
        c(-119, 36),
        c(-120, 36),
        c(-120, 35)
    ))),
    crs = 4326
))

# Create a valid point as an sf object
point_sf <- data.frame(lon = -120, lat = 36) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bad_point_sf <- data.frame(lon = -180, lat = 0) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

test_that("download.caladapt.loca returns a tibble with expected columns for polygon input", {
  skip_on_cran()
  skip_on_ci()
  tmp_dir <- withr::local_tempdir()
  result <- download.caladapt.loca(
        outfolder = tmp_dir,
        start_date = 2001,
        end_date = 2003,
        sf_obj = polygon_sf,
        var = "pr",
        gcm = "HadGEM2-ES",
        scenario = "historical",
        period = "year"
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

test_that("download.caladapt.loca returns a tibble with expected columns for point input", {
    skip_on_cran()
    skip_on_ci()
    tmp_dir <- withr::local_tempdir()
    result <- download.caladapt.loca(
        outfolder = tmp_dir,
        start_date = 2001,
        end_date = 2003,
        sf_obj = point_sf,
        var = "tasmax",
        gcm = "HadGEM2-ES",
        scenario = "historical",
        period = "year"
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


test_that("download.caladapt.loca handles SpatVector objects", {
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("terra")
    
    # Create a terra SpatVector from the sf object
    spat_point <- terra::vect(point_sf)
    
    tmp_dir <- withr::local_tempdir()
    result <- download.caladapt.loca(
        outfolder = tmp_dir,
        start_date = 2001,
        end_date = 2003,
        sf_obj = spat_point,
        var = "tasmax",
        gcm = "HadGEM2-ES",
        scenario = "historical",
        period = "year"
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

## test that downloaded files are valid
tmp_dir <- withr::local_tempdir()
result <- download.caladapt.loca(
    outfolder = tmp_dir,
    start_date = 2001,
    end_date = 2003,
    sf_obj = polygon_sf,
    var = "pr",
    gcm = "HadGEM2-ES",
    scenario = "historical",
    period = "year"
)

test_that("download.caladapt.loca returns a valid raster", {
    skip_on_cran()
    skip_on_ci()
    expect_true(all(file.exists(result$raster)))
    s <- stars::read_stars(result$raster)
    expect_s3_class(s, "stars")
    expect_true(all(dim(s) > 0))
})