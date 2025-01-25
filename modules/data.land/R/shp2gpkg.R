#' Convert Shapefile to GeoPackage
#'
#' This function converts a Shapefile to a GeoPackage using the `sf` package.
#'
#' @param input_shp Character. Path to the input Shapefile (e.g., `"data/myfile.shp"`).
#' @param output_gpkg Character. Path to the output GeoPackage (e.g., `"output/myfile.gpkg"`).
#' @param layer_name Character. Optional. Name of the layer in the GeoPackage. Defaults to the base name of the Shapefile.
#' @param overwrite Logical. Whether to overwrite an existing layer in the GeoPackage. Defaults to `TRUE`.
#'
#' @return Invisibly returns the path to the GeoPackage (`output_gpkg`) upon successful conversion.
#' @details
#' This function reads a Shapefile, converts it to an `sf` object, and writes it to a GeoPackage.
#' The `sf` package handles the conversion and ensures spatial data integrity.
#'
#' @examples
#' # Convert 'roads.shp' to 'roads.gpkg' with the default layer name
#' shp2gpkg("data/roads.shp", "output/roads.gpkg")
#'
#' # Convert with a custom layer name
#' shp2gpkg("data/roads.shp", "output/roads.gpkg", layer_name = "custom_layer")
#'
#' # Prevent overwriting existing layers
#' shp2gpkg("data/roads.shp", "output/roads.gpkg", overwrite = FALSE)
#'
#' @importFrom sf st_read st_write
#' @export
shp2gpkg <- function(input_shp, output_gpkg, layer_name = NULL, overwrite = TRUE) {
  # Load the Shapefile
  shapefile <- sf::st_read(input_shp, quiet = TRUE)

  # Check validity of geometries
  is_geometry_valid <- st_is_valid(shapefile)
  total_geometries <- length(is_geometry_valid)
  n_invalid <- sum(!is_geometry_valid)
  percent_invalid <- (n_invalid / total_geometries) * 100

  # Report invalid geometry statistics
  message("Total geometries: ", total_geometries)
  message("Invalid geometries: ", n_invalid)
  message("Percentage invalid: ", round(percent_invalid, 2), "%")

  if (n_invalid > 0) {
    # Attempt to repair invalid geometries
    repaired_shapefile <- st_make_valid(shapefile)
    is_repair_successful <- st_is_valid(repaired_shapefile)
    n_repaired <- n_invalid - sum(!is_repair_successful)
    n_remaining_invalid <- sum(!is_repair_successful)

    # Report repair results
    message("Repaired ", n_repaired, " geometries successfully.")
    if (n_remaining_invalid > 0) {
      message("Could not repair ", n_remaining_invalid, " geometries. They will be excluded.")
    } else {
      message("All geometries were repaired successfully.")
    }

    # Retain only valid geometries after repair
    shapefile <- repaired_shapefile[is_repair_successful, ]
  }

  # Determine layer name from input file if not provided
  if (is.null(layer_name)) {
    layer_name <- tools::file_path_sans_ext(basename(input_shp))
  }

  # Write to GeoPackage
  st_write(
    shapefile,
    output_gpkg,
    layer = layer_name,
    delete_layer = overwrite,
    quiet = TRUE
  )

  # Invisibly return the file path
  invisible(output_gpkg)
}



#' Convert a LandIQ GeoPackage to a Standardized GeoPackage Format
#'
#' This function reads a GeoPackage created by shp2gpkg with LandIQ crop map
#' as input, extracts centroids for latitude and longitude,
#' converts area from acres to hectares, and writes the processed data to
#' a new GeoPackage.
#'
#' @param input_gpkg Character. Path to the input GeoPackage.
#' @param output_gpkg Character. Path to the output GeoPackage
#'
#' @return Invisibly returns the path to the output GeoPackage (`output_gpkg`) upon successful processing.
#' @details
#' This function reads a GeoPackage using the `sf` package, calculates the centroids to extract
#' latitude and longitude, converts the `Acres` column to hectares (`ha`), assigns the `Crop2016`
#' column to `crop`, and sets a constant value for `pft` as `'woody perennial crop'`.
#' The processed data is then saved to a new GeoPackage.
#'
#' @examples
#' # Specify input and output file paths
#' input_gpkg <- "path/to/your_input.gpkg"
#' output_gpkg <- "path/to/your_output.gpkg"
#'
#' # Run the conversion function
#' convert_gpkg(input_gpkg, output_gpkg)
#'
#' @importFrom sf st_read st_transform st_centroid st_coordinates st_write
#' @importFrom dplyr mutate select
#' @export
convert_landiq_gpkg <- function(input, output_gpkg) {
  # Check if shp or gpkg; if shp run shp2gpkg
  if (grepl(pattern = ".shp$", input)) {
    input_gpkg <- shp2gpkg(input, output_gpkg)
  } else if (grepl(pattern = ".gpkg$", input)) {
    input_gpkg <- input
  } else {
    PEcAn.logger::logger.error(
      "Input file must be a Shapefile (.shp) or GeoPackage (.gpkg)."
    )
  }
  # Read the GeoPackage
  gpkg <- sf::st_read(input_gpkg, quiet = TRUE)

  # Find column with name CropYYYY
  cropcol <- colnames(gpkg) |>
    grep(pattern = "Crop[0-9]{4}", value = TRUE)
  year <- gsub("Crop", "", cropcol)

  # Transform to WGS 84 (EPSG:4326) for lat/lon and perform mutations
  gpkg <- gpkg |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      lon = sf::st_coordinates(sf::st_centroid(geom))[, "X"],
      lat = sf::st_coordinates(sf::st_centroid(geom))[, "Y"],
      ha = PEcAn.utils::ud_convert(Acres, "acres", "ha"),
      year = year,
      crop = .data[[cropcol]],
      pft = dplyr::case_when(
        crop %in% c(
          "Cherries", "Almonds", "Plums, Prunes and Apricots",
          "Walnuts", "Citrus", "Miscellaneous Deciduous", "Pears",
          "Olives", "Apples", "Pistachios", "Bush Berries",
          "Peaches/Nectarines", "Miscellaneous Subtropical Fruits",
          "Pomegranates"
        )
        ~ "woody perennial crop",
        # add additional PFT mappings here;
        # perhaps move to CSV
        TRUE ~ paste0("no PFT for ", crop)
      )
    ) |>
    dplyr::rename(
      geometry = geom,
      source = Source,
      notes = Comments
    ) |>
    dplyr::select(
      geometry, lat, lon, year, crop,
      pft, ha, source, notes
    )
  # - geometry
  # - latitude
  # - longitude
  # - year
  # - crop
  # - plant_functional_type
  # - area_ha
  # - source
  # - notes

  sf::st_write(gpkg, output_gpkg, delete_layer = TRUE)

  # invisibly return the file path
  invisible(output_gpkg)
}
