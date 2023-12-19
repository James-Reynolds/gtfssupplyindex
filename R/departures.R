#' Determine the number of departures from a vector of stop ids on a specific date between two times
#'
#' @param gtfs A tidygtfs containing the timetable information.
#' @param stop_ids A vector of strings with the stop id numbers from the gtfs.
#' @param date_as_ymd A character string describing the date of interest in yms format.
#' @param start_as_hms A character string describing the start time of the period of interest, timezone local to the gtfs files.
#' @param end_as_hms A character string describing the end time of the period of interest, timezone local to the gtfs files.
#'
#' @return A number.
#'
#' @examples
library(tidyverse)
library(tidytransit)

#' @export
departures <- function(gtfs = gtfs,
                       stop_ids = stop_ids,
                       date_as_ymd = date_as_ymd,
                       start_as_hms = lubridate::hms("-1:0:0"),
                       end_as_hms = lubridate::hms("48:00:00")) {
  # This is based on the tidytransit vignette on producing a departure timetable at https://r-transit.github.io/tidytransit/articles/timetable.html
  # Trips departing from stop-----------------------------------
  # Join to the selected stop_ids the trip columns: route_id, service_id, trip_headsign, and trip_origin. Because stop_ids and trips are linked via the stop_times data frame, we do this by joining the stop_ids we’ve selected to the stop_times data frame and then to the trips data frame.

  departures <- stop_ids %>%
    dplyr::inner_join(
      gtfs$stop_times %>%
        dplyr::select(
          trip_id, arrival_time,
          departure_time, stop_id
        ),
      by = "stop_id"
    )

  departures <- departures %>%
    dplyr::left_join(
      gtfs$trips %>%
        dplyr::select(
          trip_id, route_id,
          service_id
        ),
      by = "trip_id"
    )

  # Extract a single day--------------------------------
  services_on_date <- gtfs$.$dates_services %>%
    dplyr::filter(date == date_as_ymd) %>%
    dplyr::select(service_id)

  departures_on_date <- departures %>%
    dplyr::inner_join(services_on_date, by = "service_id")

  # return count of number of rows in departures_on_date dataframe between the start and end times
  nrow(departures_on_date %>%
    dplyr::filter(departure_time >= start_hms) %>%
    dplyr::filter(departure_time < end_hms))
}
