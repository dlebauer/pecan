#' Make some values into an NCDF dimension variable
#'
#' Units and longnames are looked up from the \code{\link{standard_vars}} table.
#'
#' @param dimname character; standard dimension name.
#' @param vals vector of dimension coordinate values (non-empty).
#' @return ncdf4::ncdim_def object
#' @examples
#' \dontrun{
#'   time_dim <- to_ncdim("time", as.numeric(Sys.time()) + 0:23 * 3600)
#'   lat_dim  <- to_ncdim("latitude", 45.5)
#' }
#' @export
#' @author Anne Thomas, David LeBauer
to_ncdim <- function(dimname, vals) {
  if (missing(dimname) || !is.character(dimname) || length(dimname) != 1) {
    PEcAn.logger::logger.severe("dimname must be length-1 character.")
  }
  if (is.null(vals) || length(vals) == 0) {
    PEcAn.logger::logger.severe(paste("Missing vals for dim", dimname, ", please check input"))
  }
  # New: ensure numeric (coercible)
  if (!is.numeric(vals)) {
    vals_num <- suppressWarnings(as.numeric(vals))
    if (any(is.na(vals_num))) {
      PEcAn.logger::logger.severe(paste("vals for", dimname, "must be numeric or coercible to numeric."))
    } else {
      PEcAn.logger::logger.warn(paste("Coerced vals for", dimname, "to numeric."))
      vals <- vals_num
    }
  }
  dim_row <- PEcAn.utils::standard_vars |> dplyr::filter(Variable.Name == dimname)
  if (nrow(dim_row) == 0) {
    PEcAn.logger::logger.severe(paste("Dimension", dimname, "not in standard_vars"))
  }
  # BUGFIX: use dim_row not dim
  if (!identical(as.character(dim_row$Category), "Dimension")) {
    PEcAn.logger::logger.severe(paste(dimname, "not a dimension or is deprecated"))
  }
  units <- as.character(dim_row$Units)
  longname <- as.character(dim_row$Long.name)
  # create_dimvar heuristic (retain previous behavior but safer for scalar)
  create_dimvar <- length(vals) > 1
  ncdim <- ncdf4::ncdim_def(
    name = dimname,
    vals = vals,
    units = units,
    longname = longname,
    create_dimvar = create_dimvar
  )
  return(ncdim)
} # to_ncdim

#' Define an NCDF variable from PEcAn standard_vars
#'
#' @param varname character; standard variable name.
#' @param dims named list of ncdim objects (may include extras).
#' @param missval numeric missing value (default -999).
#' @return ncdf4::ncvar_def object
#' @examples
#' \dontrun{
#'   tdim <- to_ncdim("time", 1:24)
#'   ldim <- to_ncdim("latitude", 45.5)
#'   x <- to_ncvar("air_temperature", list(time = tdim, latitude = ldim))
#' }
#' @export
#' @author Anne Thomas, David LeBauer
to_ncvar <- function(varname, dims, missval = -999) {
  if (missing(varname) || !is.character(varname) || length(varname) != 1) {
    PEcAn.logger::logger.severe("varname must be length-1 character.")
  }
  nc_row <- PEcAn.utils::standard_vars[which(PEcAn.utils::standard_vars$Variable.Name == varname), ]
  if (nrow(nc_row) == 0) {
    PEcAn.logger::logger.severe(paste("Variable", varname, "not in standard_vars"))
  }
  # Required dimension names (preserve order, drop NAs / empty)
  dim_names <- na.omit(unlist(nc_row[, c("dim1", "dim2", "dim3", "dim4")], use.names = FALSE))
  dim_names <- dim_names[dim_names != ""]
  if (length(dim_names) == 0) {
    PEcAn.logger::logger.severe(paste("No dimension metadata found for", varname))
  }
  if (is.null(dims) || !is.list(dims) || is.null(names(dims))) {
    PEcAn.logger::logger.severe("dims must be a named list of ncdim objects.")
  }
  # Order dims according to standard_vars
  missing_dims <- setdiff(dim_names, names(dims))
  if (length(missing_dims)) {
    PEcAn.logger::logger.severe(
      paste("Missing required dimensions for", varname, ":", paste(missing_dims, collapse = ", "))
    )
  }
  if (length(setdiff(names(dims), dim_names)) > 0) {
    PEcAn.logger::logger.warn(
      paste("Ignoring extra dims for", varname, ":", paste(setdiff(names(dims), dim_names), collapse = ", "))
    )
  }
  ordered_dims <- dims[dim_names]
  # New: class check for each required dim
  bad_dims <- vapply(dims[dim_names], function(d) !inherits(d, "ncdim4"), logical(1)) # ncdf4 uses class 'ncdim4'
  if (any(bad_dims)) {
    PEcAn.logger::logger.severe(
      paste("One or more provided dims for", varname, "are not ncdf4 dimension objects:",
            paste(names(bad_dims)[bad_dims], collapse = ", "))
    )
  }
  units <- as.character(nc_row$Units)
  longname <- as.character(nc_row$Long.name)
  ncvar <- ncdf4::ncvar_def(
    name = varname,
    units = units,
    longname = longname,
    dim = ordered_dims,
    missval = missval,
    prec = "double"
  )
  return(ncvar)
} # to_ncvar