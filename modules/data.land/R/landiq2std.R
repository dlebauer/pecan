#' Convert a LandIQ Shapefile into a Standardized Format consisting of
#' a GeoPackage file to store geospatial information and an associated
#' CSV file with attributes
#'
#' This function reads a LandIQ crop map shapefile downloaded from
#' https://data.cnra.ca.gov/dataset/statewide-crop-mapping and processes the data into
#' a standardized GeoPackage and CSV format.
#'
#' @param input_file Character. Path to the input Shapefile
#' (GeoPackage created by shp2gpkg is also valid)
#' @param output_gpkg Character. Path to the output GeoPackage
#' @param output_csv Character. Path to the output CSV.
#' @param overwrite Logical. If TRUE, overwrites existing files.
#'
#' @return Invisibly returns a list with paths to the output files.
#' @details
#' This function:
#' - Reads a Shapefile using the `sf` package.
#' - Calculates centroids to extract latitude and longitude.
#' - Converts `Acres` column to hectares (`ha`).
#' - Extracts crop information and assigns a PFT (Plant Functional Type).
#' - Outputs a standardized GeoPackage (geometry in California Albers,
#'   EPSG:3310) with columns `id`, `geometry`, `lat`, `lon`, and `area_ha`.
#' - Outputs a CSV with columns `year`, `crop`, `pft`, `source`, and `notes`.
#'
#' Note: TODO provide crop-->PFT mapping as an external file using either
#' defaults stored with the PEcAn.data.land package or an optional custom
#' mapping
#'
#' @examples
#' # Specify input and output file paths
#' input_file <- "path/to/landiq.shp"
#' output_gpkg <- "path/to/your_output.gpkg"
#' output_csv <- "path/to/your_output.csv"
#'
#' # Run the conversion function
#' landiq2std(input_file, output_gpkg, output_csv)
#'
#' @export
landiq2std <- function(input_file, output_gpkg, output_csv, overwrite = TRUE) {
  # Check input file format
  # If shapefile, convert to GeoPackage
  if (grepl(pattern = "\\.shp$", input_file)) {
    # read in, repair geometries, write out repaired geopackage
    tempfile <- tempfile(fileext = ".gpkg")
    shp2gpkg(input_file, tempfile, overwrite = TRUE)
    input_file <- tempfile # now gpkg is input file
    on.exit(unlink(input_file), add = TRUE)
  }

  # Read the Shapefile
  landiq_polygons <- sf::st_read(input_file, quiet = FALSE)

  # Determine crop year from column name
  crop_col <- grep("Crop[0-9]{4}", colnames(landiq_polygons), value = TRUE)
  year <- gsub("Crop", "", crop_col)
  # Check required columns
  required_cols <- c("Acres", crop_col, "Source", "Comments", "County", "geom")
  missing_cols <- setdiff(required_cols, colnames(landiq_polygons))
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.error("Input file is missing the following columns: ", paste(missing_cols, collapse = ", "))
  }

  landiq_polygons_updated <- landiq_polygons |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      coords = sf::st_coordinates(sf::st_centroid(geom))
    ) |>
    dplyr::mutate(
      year = year,
      lon = coords[, "X"],
      lat = coords[, "Y"],
      area_ha = PEcAn.utils::ud_convert(Acres, "acre", "ha")
    ) |>
    dplyr::select(-coords) |>
    # digest step used to create unique site_ids requires rowwise
    dplyr::rowwise() |>
    mutate(
      site_id = digest::digest(geom, algo = "xxhash64")
    ) |>
    dplyr::rename(county = County)

  # Process data for GeoPackage in California Albers (EPSG:3310)
  gpkg_data <- landiq_polygons_updated |>
    dplyr::select(site_id, geom, lat, lon, area_ha, county) |>
    sf::st_transform(3310)   # EPSG:3310 = NAD83 / California Albers

  # Process data for CSV
  csv_data <- landiq_polygons_updated |>
    tidyr::as_tibble() |>
    dplyr::mutate(
      crop = .data[[crop_col]]
    ) |>
    # join to external lookup table for pft
    dplyr::left_join(landiq_pft_map, by = "crop") |>
    # default any missing pft to "annual crop"
    dplyr::mutate(
      pft = dplyr::coalesce(pft, "annual crop")
    ) |>
    dplyr::rename(
      source = Source,
      notes = Comments
    ) |>
    dplyr::select(site_id, lat, lon, year, crop, pft, source, notes)

  # Warn about crops without a PFT
  unassigned_pft <- csv_data |>
    dplyr::filter(grepl("no PFT for", pft)) |>
    dplyr::distinct(crop, pft)
  if (nrow(unassigned_pft) > 0) {
    PEcAn.logger::logger.warn( 
      "The following crops do not have a PFT assigned:",
      paste(unassigned_pft$crop, collapse = ", ")
    )
  }

  # Write outputs
  if(!overwrite) {
    if (file.exists(output_gpkg)) {
      PEcAn.logger::logger.error("Output GeoPackage already exists. Set overwrite = TRUE to overwrite.")
    }
    if (file.exists(output_csv)) {
      PEcAn.logger::logger.error("Output CSV already exists. Set overwrite = TRUE to overwrite.")
    }
  } else {
    if (file.exists(output_gpkg)) {
      PEcAn.logger::logger.info("Overwriting existing file", output_gpkg)
      file.remove(output_gpkg, showWarnings = FALSE)
    }
    if (file.exists(output_csv)) {
      PEcAn.logger::logger.info("Overwriting existing file", output_csv)
      file.remove(output_csv, showWarnings = FALSE)
    }
  }

  sf::st_write(gpkg_data,
    output_gpkg,
    layer = "sites", # analogous to BETYdb table name
    quiet = FALSE
  )
  readr::write_csv(csv_data, output_csv)

  # Return success status
  invisible(TRUE)
}
