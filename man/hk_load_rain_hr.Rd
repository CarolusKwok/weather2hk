% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hk_load_rain_hr.R
\name{hk_load_rain_hr}
\alias{hk_load_rain_hr}
\title{HK weather - Downloading daily rainfall distribution images from https://www.hko.gov.hk/en/index.html}
\usage{
hk_load_rain_hr(
  time = weather2::tool_datetime(end = Sys.time(), by = "15 min", duration = "7 day"),
  lan = "en",
  list_fail = T,
  dir = getwd(),
  attempt = 5L,
  worker = 1L
)
}
\arguments{
\item{time}{Date/time to download}

\item{lan}{Language. Accepts "en", "tc", "sc".}

\item{list_fail}{List failed-to-download items}

\item{dir}{Directory of downloaded data.}

\item{attempt}{Attempts to be made per file to download}

\item{worker}{Numbers of sessions to be open}
}
\description{
HK weather - Downloading daily rainfall distribution images from https://www.hko.gov.hk/en/index.html
}
\examples{
hk_load_rain_hr()
}
