% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_restart.LPJGUESS.R
\name{write_restart.LPJGUESS}
\alias{write_restart.LPJGUESS}
\title{write_restart.LPJGUESS}
\usage{
write_restart.LPJGUESS(
  outdir,
  runid,
  start.time,
  stop.time,
  settings,
  new.state,
  RENAME = TRUE,
  new.params,
  inputs = NULL,
  verbose = FALSE
)
}
\arguments{
\item{outdir}{output directory}

\item{runid}{run ID}

\item{start.time}{start date and time for each SDA ensemble}

\item{stop.time}{stop date and time for each SDA ensemble}

\item{settings}{PEcAn settings object}

\item{new.state}{analysis state vector}

\item{RENAME}{flag to either rename output file or not}

\item{new.params}{list of parameters to convert between different states}

\item{inputs}{list of model inputs to use in write.configs.SIPNET}

\item{verbose}{decide if we want to print the runid}
}
\value{
NONE
}
\description{
Write restart files for LPJGUESS
new.state includes X (AGB.pft) from Analysis
new.params includes LPJGUESS_state
}
\author{
Yinghao Sun
}
