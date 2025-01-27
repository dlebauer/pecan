#' Convert a LandIQ Shapefile into a Standardized Format consisting of 
#' a GeoPackage file to store geospatial information and an associated
#' CSV file with attributes 
#'
#' This function reads a LandIQ crop map shapefile downloaded from
#' https://www.landiq.com/land-use-mapping and processes the data into
#' a standardized GeoPackage and CSV format.
#'
#' @param input_shp Character. Path to the input Shapefile
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
#' input_shp <- "path/to/landiq.shp"
#' output_gpkg <- "path/to/your_output.gpkg"
#' output_csv <- "path/to/your_output.csv"
#'
#' # Run the conversion function
#' landiq2std(input_shp, output_gpkg, output_csv)
#'
#' @importFrom sf st_read st_transform st_centroid st_coordinates st_write
#' @importFrom dplyr mutate select rename filter case_when distinct
#' @export
landiq2std <- function(input_shp, output_gpkg, output_csv) {
  
  # Check input file format
  if (!grepl(pattern = "\\.shp$", input_shp)) {
    stop("Input file must be a Shapefile (.shp).")
  }
  
  # Read the Shapefile
  shp_data <- sf::st_read(input_shp, quiet = TRUE)
  
  # Check required columns
  required_cols <- c("Acres", "Crop2016", "Source", "Comments")
  missing_cols <- setdiff(required_cols, colnames(shp_data))
  if (length(missing_cols) > 0) {
    stop("Input Shapefile is missing the following columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Determine crop year from column name
  crop_col <- grep("Crop[0-9]{4}", colnames(shp_data), value = TRUE)
  year <- gsub("Crop", "", crop_col)
  
  shp_data <- shp_data |>
    sf::st_transform(4326) |>
    rename(geometry = geom) |>
    mutate(
      id = digest::digest(geometry, algo = 'xxhash64'),
      lon = sf::st_coordinates(sf::st_centroid(geometry))[, "X"],
      lat = sf::st_coordinates(sf::st_centroid(geometry))[, "Y"],
      area_ha = PEcAn.utils::ud_convert(Acres, 'acre', 'ha')
    )
  
  # Process data for GeoPackage
  gpkg_data <- shp_data |>
    select(id, geometry, lat, lon, area_ha)
  
  # Process data for CSV
  csv_data <- shp_data |>
    mutate(
      crop = .data[[crop_col]],
      pft = case_when(
        crop %in% c("Cherries", "Almonds", "Plums, Prunes and Apricots",
                    "Walnuts", "Citrus", "Miscellaneous Deciduous", "Pears", "Olives", 
                    "Apples", "Pistachios", "Bush Berries", "Peaches/Nectarines", 
                    "Miscellaneous Subtropical Fruits", "Pomegranates") ~ "woody perennial crop",
        TRUE ~ paste0("no PFT for ", crop)
      ),
      source = Source,
      notes = Comments
    ) |>
    select(id, year, crop, pft, source, notes)
  
  # Warn about crops without a PFT
  unassigned_pft <- csv_data |>
    filter(grepl("no PFT for", pft)) |>
    distinct(crop, pft)
  if (nrow(unassigned_pft) > 0) {
    PEcAn.logger::logger.warn( # or should this be error?
      "The following crops do not have a PFT assigned:\n", 
      paste(unassigned_pft$crop, collapse = ", ")
    )
  }
  
  # Write outputs
  sf::st_write(gpkg_data, output_gpkg, delete_layer = TRUE, quiet = TRUE)
  readr::write_csv(csv_data, output_csv, row.names = FALSE)
  
  # Return paths to output files
  invisible(list(GeoPackage = output_gpkg, CSV = output_csv))
}
