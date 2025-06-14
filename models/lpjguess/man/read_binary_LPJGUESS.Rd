% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_state.R
\name{read_binary_LPJGUESS}
\alias{read_binary_LPJGUESS}
\title{Read Binary File for LPJ-GUESS}
\usage{
read_binary_LPJGUESS(outdir, version = "PalEON")
}
\arguments{
\item{outdir}{The output directory where ".state" and "meta.bin" will be written}

\item{version}{A character string specifying the LPJ-GUESS version (default is "PalEON").}
}
\value{
A matrix or list containing the extracted data.
}
\description{
Reads a binary file formatted for LPJ-GUESS and extracts relevant data.
}
\author{
Istem Fer, Yinghao Sun
}
