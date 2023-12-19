#' Determine the number of departures for a single mode at a stop between two time and dates
#'
#' @param gtfs A tidygtfs containing the timetable information.
#' @param stop_ids A vector of strings with the stop id numbers from the gtfs.
#' @param date_as_ymd A character string describing the date of interest in yms format.
#' @param start_as_hms A character string describing the start time of the period of interest, timezone local to the gtfs files.
#' @param end_as_hms A character string describing the end time of the period of interest, timezone local to the gtfs files.
#' @param route_type Numeric describing the type of transportation the number of departures is to be calculated for, based on the GTFS static definitions.
#'
#' @return
#' @export
#'
#' @examples
departures_one_day <- function(gtfs = gtfs, stop_ids = stop_ids, date_as_ymd = date_as_ymd, start_as_hms = start_as_hms, end_as_hms = end_as_hms, route_type) {

#This is based on the tidytransit vignette on producing a departure timetable at https://r-transit.github.io/tidytransit/articles/timetable.html


#Temporary used while building the function-----
library(tidyverse)
library(tidytransit)


#Read  test data------------------------------------
#We use a feed from the New York Metropolitan Transportation Authority. It is provided as a sample feed with tidytransit but you can read it directly from the MTA’s website.
local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)

#To create a departure timetable, we first need to find the ids of all stops in the stops table with the same same name, as stop_name might cover different platforms and thus have multiple stop_ids in the stops table.
stop_ids <- gtfs$stops %>%
  filter(stop_name == "Times Sq - 42 St") %>%
  select(stop_id)
#Note that multiple unrelated stops can have the same stop_name, see cluster_stops() for examples how to find these cases

date_as_ymd <- "2018-08-23"
start_hms <- hms("7:00:00")
end_hms <- hms("7:10:00")
#route_type


#Trip origin and headsign---------------------------
#To display where a bus (or any public transit vehicle) is headed on a timetable we need the column trip_headsign in gtfs$trips. This is an optional field but our example feed provides this information. To display where a vehicle comes from on the timetable we need to create a new column in gtfs$trips which we’ll call trip_origin.

# get the id of the first stop in the trip's stop sequence
first_stop_id <- gtfs$stop_times %>%
  group_by(trip_id) %>%
  summarise(stop_id = stop_id[which.min(stop_sequence)])

# join with the stops table to get the stop_name
first_stop_names <- left_join(first_stop_id, gtfs$stops, by="stop_id")

# rename the first stop_name as trip_origin
trip_origins <- first_stop_names %>% select(trip_id, trip_origin = stop_name)

# join the trip origins back onto the trips
gtfs$trips <- left_join(gtfs$trips, trip_origins, by = "trip_id")

gtfs$trips %>%
  select(route_id, trip_origin) %>%
  head()

##In case trip_headsign does not exist in the feed it can be generated similarly to trip_origin:

  if(!exists("trip_headsign", where = gtfs$trips)) {
    # get the last id of the trip's stop sequence
    trip_headsigns <- gtfs$stop_times %>%
      group_by(trip_id) %>%
      summarise(stop_id = stop_id[which.max(stop_sequence)]) %>%
      left_join(gtfs$stops, by="stop_id") %>%
      select(trip_id, trip_headsign.computed = stop_name)

    # assign the headsign to the gtfs object
    gtfs$trips <- left_join(gtfs$trips, trip_headsigns, by = "trip_id")
  }

#Trips departing from stop-----------------------------------
#To the selected stop_ids for Time Square, we can join trip columns: route_id, service_id, trip_headsign, and trip_origin. Because stop_ids and trips are linked via the stop_times data frame, we do this by joining the stop_ids we’ve selected to the stop_times data frame and then to the trips data frame.

departures <- stop_ids %>%
  inner_join(gtfs$stop_times %>%
               select(trip_id, arrival_time,
                      departure_time, stop_id),
             by = "stop_id")

departures <- departures %>%
  left_join(gtfs$trips %>%
              select(trip_id, route_id,
                     service_id, trip_headsign,
                     trip_origin),
            by = "trip_id")

#add route info (route_short_name)------------------------
#Each trip belongs to a route, and the route short name can be added to the departures by joining the trips data frame with gtfs$routes.

departures <- departures %>%
  left_join(gtfs$routes %>%
              select(route_id,
                     route_short_name),
            by = "route_id")

#Extract a single day--------------------------------
#Now we are ready to extract the same service table for any given day of the year.


services_on_date <- gtfs$.$dates_services %>%
  filter(date == date_as_ymd) %>% select(service_id)

departures_on_date <- departures %>%
  inner_join(services_on_date, by = "service_id")


departures_in_period <- departures_on_date %>%
  arrange(departure_time, stop_id, route_short_name) %>%
  select(departure_time, stop_id, route_short_name, trip_headsign) %>%
  filter(departure_time >= start_hms) %>%
  filter(departure_time < end_hms)

departures_in_period
nrow(departures_in_period)


}


