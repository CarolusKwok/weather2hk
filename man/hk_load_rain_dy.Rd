% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hk_load_rain_dy.R
\name{hk_load_rain_dy}
\alias{hk_load_rain_dy}
\title{HK weather - Downloading daily rainfall distribution image from https://www.hko.gov.hk/en/index.html}
\usage{
hk_load_rain_dy(
  lan = "en",
  list_fail = T,
  dir = getwd(),
  attempt = 5L,
  worker = 1L
)
}
\arguments{
\item{lan}{Language. Accepts "en", "tc", "sc"}

\item{list_fail}{List failed-to-download items}

\item{dir}{Directory of downloaded data.}

\item{attempt}{Attempts to be made per file to download}

\item{worker}{Numbers of sessions to be open}
}
\description{
HK weather - Downloading daily rainfall distribution image from https://www.hko.gov.hk/en/index.html
}
\examples{
hk_load_rain_dy()
}
