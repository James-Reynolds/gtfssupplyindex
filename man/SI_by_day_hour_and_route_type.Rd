% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SI_by_day_hour_and_route_type.R
\name{SI_by_day_hour_and_route_type}
\alias{SI_by_day_hour_and_route_type}
\title{Calculate SI for each: route type, hour (of service), day between
a start date and an end date (inclusive), and area_of_interest}
\usage{
SI_by_day_hour_and_route_type(
  list_gtfs,
  stops_in_or_near_areas,
  start_date_ymd,
  end_date_ymd,
  verbose = FALSE
)
}
\arguments{
\item{list_gtfs}{Output of the gtfs_by_route_type function,
a list (by route_type) of tidygtfs objects}

\item{stops_in_or_near_areas}{Output of the stops_in_walk_dist function.
A list (by route_type) of stops and areas within walking distance of each other
and the associated area_terms of SI calculation}

\item{start_date_ymd}{the first day of the period of interest}

\item{end_date_ymd}{the last day of the period of interest}

\item{verbose}{Print details of progress during calculations}
}
\value{
A dataframe with SI for each combination of route type, area of interest,
hour of service and date.
}
\description{
Calculate SI for each: route type, hour (of service), day between
a start date and an end date (inclusive), and area_of_interest
}
\examples{
####-----first load all the inputs
#load the revised mornington GTFS data
list_gtfs = gtfssupplyindex:::gtfs_by_route_type(system.file(
"extdata/mornington180109",
"gtfs.zip", 
package = "gtfssupplyindex", 
mustWork = TRUE))

areas_of_interest <- load_areas_of_interest(absmapsdata::sa22021 \%>\% 
            dplyr::filter(sa3_name_2021 ==  "Mornington Peninsula") \%>\% 
                                             dplyr::select(sa2_code_2021),  
                                           area_id_field = "sa2_code_2021") 
buffer_distance <- gtfssupplyindex:::load_buffer_zones()

stops_in_or_near_areas <- gtfssupplyindex:::stops_in_walk_dist(
list_gtfs = list_gtfs, 
areas_of_interest = areas_of_interest,
EPSG_for_transform = 28355,
verbose = FALSE
)

SI_by_day_hour_and_route_type(
list_gtfs, 
stops_in_or_near_areas, 
"2018-12-23",
"2018-12-30",
verbose = FALSE)
}
