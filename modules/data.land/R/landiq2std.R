#' Convert a LandIQ Shapefile into a Standardized Format consisting of
#' a GeoPackage file to store geospatial information and an associated
#' CSV file with attributes
#'
#' This function reads a LandIQ crop map shapefile downloaded from
#' https://www.landiq.com/land-use-mapping and processes the data into
#' a standardized GeoPackage and CSV format.
#'
#' @param input_file Character. Path to the input Shapefile
#' (GeoPackage created by shp2gpkg is also valid)
#' @param output_gpkg Character. Path to the output GeoPackage
#' @param output_csv Character. Path to the output CSV.
#'
#' @return Invisibly returns a list with paths to the output files.
#' @details
#' This function:
#' - Reads a Shapefile using the `sf` package.
#' - Calculates centroids to extract latitude and longitude.
#' - Converts `Acres` column to hectares (`ha`).
#' - Extracts crop information and assigns a PFT (Plant Functional Type).
#' - Outputs a standardized GeoPackage with columns `id`, `geometry`, `lat`, `lon`, and `area_ha`.
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
#' @importFrom sf st_read st_transform st_centroid st_coordinates st_write
#' @importFrom dplyr mutate select rename filter case_when distinct
#' @export
landiq2std <- function(input_file, output_gpkg, output_csv) {
  # Check input file format
  if (grepl(pattern = "\\.shp$", input_file)) {
    PEcAn.logger::logger.info(
      "Converting Shapefile:", basename(input_file),
      "to GeoPackage:", basename(output_gpkg)
    )
    # read in, repair geometries, write out repaired geopackage
    shp2gpkg(input_file, output_gpkg, overwrite = TRUE)
    input_file <- output_gpkg # now gpkg is input file
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

  # possible spedup by pre-computing lat and lon
  landiq_polygons_updated <- landiq_polygons |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      year = year,
      lon = sf::st_coordinates(sf::st_centroid(geom))[, "X"],
      lat = sf::st_coordinates(sf::st_centroid(geom))[, "Y"],
      area_ha = PEcAn.utils::ud_convert(Acres, "acre", "ha")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate( # generate ids rowwise separately because
      # rowwise geospatial operations are very slow
      id = digest::digest(geom, algo = "xxhash64")
    ) |>
    dplyr::rename(county = County)

  # Process data for GeoPackage
  gpkg_data <- landiq_polygons_updated |>
    dplyr::select(id, geom, lat, lon, area_ha, county)

  # Process data for CSV
  csv_data <- landiq_polygons_updated |>
    tidyr::as_tibble() |>
    dplyr::mutate(
      crop = .data[[crop_col]],
      pft = case_when(
        crop %in% c(
          "Cherries", "Almonds", "Plums, Prunes and Apricots",
          "Walnuts", "Citrus", "Miscellaneous Deciduous", "Pears", "Olives",
          "Apples", "Pistachios", "Bush Berries", "Peaches/Nectarines",
          "Miscellaneous Subtropical Fruits", "Pomegranates"
        ) ~ "woody perennial crop",
        TRUE ~ paste0("no PFT for ", crop)
      )
    ) |>
    dplyr::rename(
      source = Source,
      notes = Comments
    ) |>
    dplyr::select(id, lat, lon, year, crop, pft, source, notes)

  # Warn about crops without a PFT
  unassigned_pft <- csv_data |>
    filter(grepl("no PFT for", pft)) |>
    distinct(crop, pft)
  if (nrow(unassigned_pft) > 0) {
    PEcAn.logger::logger.warn( 
      "The following crops do not have a PFT assigned:",
      paste(unassigned_pft$crop, collapse = ", ")
    )
  }

  # Write outputs
  file.remove(output_gpkg, output_csv)
  sf::st_write(gpkg_data,
    output_gpkg,
    layer = "sites", # analogous to BETYdb table name
    quiet = FALSE
  )
  readr::write_csv(csv_data, output_csv)

  # Return paths to output files
  invisible(list(GeoPackage = output_gpkg, CSV = output_csv))
}
