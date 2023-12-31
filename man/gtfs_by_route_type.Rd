% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs_by_route_type.R
\name{gtfs_by_route_type}
\alias{gtfs_by_route_type}
\title{tidygtfs object to list by route_type of tidygtfs object}
\usage{
gtfs_by_route_type(
  path = file.path(system.file("extdata", package = "tidytransit"),
    "google_transit_nyc_subway.zip")
)
}
\arguments{
\item{path}{to GTFS .zip file. Defaults to the NYC MTA sample gtfs from tidytransit package}
}
\value{
A list, each element being a tidygtfs for an individual mode.
}
\description{
\code{gtfs_by_route_type()} loads a GTFS into the datatable format used by gtfstool, then splits it by route_type, and returns a list with a tidygtfs for each route type
}
\examples{
# Load sample google-supplied sample gtfs from gtfstools
path <- file.path(system.file("extdata", package = "gtfstools"), "ggl_gtfs.zip")
gtfs_by_route_type(path)

# Load NYC MTA sample gtfs from tidytransit
path <- file.path(system.file("extdata", package = "tidytransit"), "google_transit_nyc_subway.zip")
gtfs_by_route_type(path)
}
