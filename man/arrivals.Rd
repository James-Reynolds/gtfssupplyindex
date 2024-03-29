% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/departures_and_arrivals.R
\name{arrivals}
\alias{arrivals}
\title{Determine the number of arrivals from a vector of stop ids on a specific date between two times}
\usage{
arrivals(
  gtfs = gtfs,
  stop_ids = stop_ids,
  date_ymd = date_ymd,
  start_hms = lubridate::hms("-1:0:0"),
  end_hms = lubridate::hms("48:00:00")
)
}
\arguments{
\item{gtfs}{A tidygtfs object containing the timetable information.}

\item{stop_ids}{A vector of strings with the stop id numbers from the gtfs.}

\item{date_ymd}{A character string describing the date of interest in yms format.}

\item{start_hms}{A character string describing the start time of the period of interest, timezone local to the gtfs. Services departing precisely at the start time are included}

\item{end_hms}{A character string describing the end time of the period of interest, timezone local to the gtfs. Services departing precisely at the end time are included}
}
\value{
A dataframe listing the number of arrivals for each stop_id in the specified time frame.
}
\description{
This function is a modification of the example shown in https://r-transit.github.io/tidytransit/articles/timetable.html
}
\examples{
gtfs <- tidytransit::read_gtfs(system.file(
"extdata/mornington180109", 
"gtfs.zip", 
package = "gtfssupplyindex", 
mustWork = TRUE))

arrivals(gtfs = gtfs, 
stop_ids = data.frame(stop_id = "1388695887"),
date_ymd = lubridate::ymd("2018-12-30"))



}
