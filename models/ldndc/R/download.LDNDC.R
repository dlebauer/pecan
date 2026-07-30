##' Download and extract the LDNDC binary distribution
##'
##' Downloads the current public LDNDC binary distribution for macOS or Linux,
##' extracts it under `location`, and returns the extracted runtime paths.
##'
##' @title Download LDNDC
##' @param location directory where the LDNDC runtime should be extracted
##' @param platform optional platform override, either `"mac64"` or `"linux64"`.
##'   By default this is inferred from `Sys.info()[["sysname"]]`.
##' @param archive optional local archive path. When supplied, the archive is
##'   extracted directly and no network download is attempted.
##' @param url optional direct archive URL. By default, PEcAn uses the public
##'   LandscapeDNDC binary distribution URL for the detected platform.
##' @param overwrite logical: replace an existing downloaded archive?
##' @return list with `root`, `binary`, `archive`, `platform`, and `source`
##' @export
download.LDNDC <- function(location = ".",
                           platform = NULL,
                           archive = NULL,
                           url = NULL,
                           overwrite = FALSE) {
  platform <- platform %||% ldndc_platform()
  url <- url %||% ldndc_download_url(platform)
  source <- archive %||% url

  location <- normalizePath(path.expand(location), mustWork = FALSE)
  if (!dir.exists(location) && !dir.create(location, recursive = TRUE)) {
    PEcAn.logger::logger.severe("Could not create LDNDC location: ", location)
  }

  if (is.null(archive)) {
    archive <- file.path(location, basename(url))
    if (!file.exists(archive) || isTRUE(overwrite)) {
      PEcAn.logger::logger.info("Downloading LDNDC from ", url)
      utils::download.file(url, archive, mode = "wb")
    }
  }

  if (!file.exists(archive)) {
    PEcAn.logger::logger.severe("LDNDC archive not found: ", archive)
  }

  extract_ldndc_archive(archive, location)
  binary <- find_ldndc_binary(location)
  root <- dirname(dirname(binary))

  Sys.chmod(binary, mode = "0755")
  clear_macos_quarantine(root, platform)

  list(
    root = normalizePath(root, winslash = "/", mustWork = FALSE),
    binary = normalizePath(binary, winslash = "/", mustWork = FALSE),
    archive = normalizePath(archive, winslash = "/", mustWork = FALSE),
    platform = platform,
    source = source
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ldndc_platform <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Darwin")) {
    return("mac64")
  }
  if (identical(sysname, "Linux")) {
    return("linux64")
  }
  PEcAn.logger::logger.severe("Unsupported LDNDC platform: ", sysname)
}

ldndc_download_url <- function(platform) {
  switch(
    platform,
    mac64 = paste0(
      "https://ldndc.imk-ifu.kit.edu/ldndc/downloads/public/packages/",
      "mac64/ldndc-1.37.mac64.2026-03-03.tar.bz2"
    ),
    linux64 = paste0(
      "https://ldndc.imk-ifu.kit.edu/ldndc/downloads/public/packages/",
      "linux64/ldndc-1.37.0.linux64.tar.bz2"
    ),
    PEcAn.logger::logger.severe("Unsupported LDNDC platform: ", platform)
  )
}

extract_ldndc_archive <- function(archive, location) {
  if (grepl("\\.zip$", archive, ignore.case = TRUE)) {
    utils::unzip(archive, exdir = location)
    return(invisible(location))
  }

  if (grepl("\\.tar(\\.(gz|bz2|xz))?$", archive, ignore.case = TRUE)) {
    utils::untar(archive, exdir = location)
    return(invisible(location))
  }

  PEcAn.logger::logger.severe("Unsupported LDNDC archive type: ", archive)
}

find_ldndc_binary <- function(location) {
  candidates <- list.files(
    location,
    pattern = "^ldndc$",
    recursive = TRUE,
    full.names = TRUE
  )
  candidates <- candidates[basename(dirname(candidates)) == "bin"]

  if (!length(candidates)) {
    PEcAn.logger::logger.severe("Could not find extracted LDNDC binary under ", location)
  }

  candidates[[1]]
}

clear_macos_quarantine <- function(root, platform) {
  if (!identical(platform, "mac64")) {
    return(invisible(FALSE))
  }

  status <- try(
    suppressWarnings(
      system2("xattr", c("-dr", "com.apple.quarantine", root), stdout = TRUE, stderr = TRUE)
    ),
    silent = TRUE
  )

  invisible(!inherits(status, "try-error"))
}
