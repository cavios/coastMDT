% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getData}
\alias{getData}
\title{Function to download data from the web}
\usage{
getData(localdir = tempdir(), files = NULL,
  url = "https://raw.githubusercontent.com/cavios/coastMDT/master/data/files/")
}
\arguments{
\item{localdir}{A character string with the name of the directory where the data will be stored. If no name is given the data is automatically stored in a temporal directory.}

\item{files}{A character vector with the names of the files to be downloaded. If not specified all data for the coastMDT package is downloaded.}

\item{url}{A character string with the url, that specifies where the data is located. If not specified, the url is where the data for the coastMDT package is located.}
}
\description{
Function to download data from the web
}
\details{
...
}
