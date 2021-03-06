% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{readAnnotsFile}
\alias{readAnnotsFile}
\title{Read one AviaNZ annotation file}
\usage{
readAnnotsFile(file)
}
\arguments{
\item{file}{Path to the JSON file. See AviaNZ documentation for current
format requirements.}
}
\value{
A dataframe (namely, tibble) with one row per each species label, and
at least 5 columns:
\itemize{
\item \code{tstart} and \code{tend} (numeric, annotation position in seconds
from the file start)
\item \code{freqmin} and \code{freqmax} (numeric, frequency bounds in Hz,
or 0, if not specified)
\item any further columns from the label, typically \code{species},
\code{certainty} and \code{filter}.
}

Note that an AviaNZ annotation may have multiple labels, corresponding to
different species; these will be parsed into individual rows.
}
\description{
Reads one AviaNZ format .data file ("new style" JSON) into an R dataframe.
}
\examples{
annotfile = system.file("extdata", "recB_230118_023355.wav.data", package="avianz2r", mustWork=TRUE)
df <- readAnnotsFile(annotfile)
}
