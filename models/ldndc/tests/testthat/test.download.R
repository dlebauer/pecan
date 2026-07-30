testthat::context("download")

make_fake_ldndc_archive <- function() {
  archive_parent <- tempfile("ldndc-archive-")
  dir.create(archive_parent, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(archive_parent, recursive = TRUE), envir = parent.frame())

  archive_root <- file.path(archive_parent, "ldndc-fake")
  dir.create(file.path(archive_root, "bin"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(archive_root, "dotfiles"), recursive = TRUE, showWarnings = FALSE)

  binary <- file.path(archive_root, "bin", "ldndc")
  writeLines("#!/bin/sh\nexit 0\n", binary)
  Sys.chmod(binary, mode = "0755")

  archive_dir <- tempfile("ldndc-tar-")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(archive_dir, recursive = TRUE), envir = parent.frame())
  archive <- file.path(archive_dir, "ldndc-fake.tar.gz")
  old_wd <- setwd(dirname(archive_root))
  on.exit(setwd(old_wd), add = TRUE)
  utils::tar(archive, files = basename(archive_root), compression = "gzip", tar = "internal")
  archive
}

testthat::test_that("download.LDNDC extracts a local archive without network", {
  archive <- make_fake_ldndc_archive()
  location <- withr::local_tempdir(pattern = "ldndc-download-")

  result <- PEcAn.LDNDC::download.LDNDC(
    location = location,
    platform = "linux64",
    archive = archive
  )

  testthat::expect_equal(result$platform, "linux64")
  testthat::expect_true(file.exists(result$binary))
  testthat::expect_equal(basename(result$root), "ldndc-fake")
})

testthat::test_that("download.LDNDC rejects unsupported platform", {
  testthat::expect_error(
    PEcAn.LDNDC::download.LDNDC(
      location = withr::local_tempdir(),
      platform = "unsupported",
      archive = make_fake_ldndc_archive()
    )
  )
})
