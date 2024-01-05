#' Determine the number of departures from a vector of stop ids on a specific date between two times
#'
#' @param gtfs A tidygtfs containing the timetable information.
#' @param stop_ids A vector of strings with the stop id numbers from the gtfs.
#' @param date_ymd A character string describing the date of interest in yms format.
#' @param start_hms A character string describing the start time of the period of interest, timezone local to the gtfs files.
#' @param end_hms A character string describing the end time of the period of interest, timezone local to the gtfs files.
#'
#' @return A number.
#'
#' @export
departures <- function(gtfs = gtfs,
                       stop_ids = stop_ids,
                       date_ymd = date_ymd,
                       start_hms = lubridate::hms("-1:0:0"),
                       end_hms = lubridate::hms("48:00:00")) {
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
    dplyr::filter(date == date_ymd) %>%
    dplyr::select(service_id)

  departures_on_date <- departures %>%
    dplyr::inner_join(services_on_date, by = "service_id")

  # return count of number of rows in departures_on_date dataframe between the start and end times
  nrow(departures_on_date %>%
    dplyr::filter(departure_time >= start_hms) %>%
    dplyr::filter(departure_time < end_hms))
}


#' @examples
#' # Read  test data------------------------------------
#' # We use a feed from the New York Metropolitan Transportation Authority. It is provided as a sample feed with tidytransit but you can read it directly from the MTA’s website.
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#'gtfs <- read_gtfs(local_gtfs_path)
#'
#' # To create a departure timetable, we first need to find the ids of all stops in the stops table with the same same name, as stop_name might cover different platforms and thus have multiple stop_ids in the stops table.
#' stop_ids <- gtfs$stops %>%
#'  dplyr::filter(stop_name == "Times Sq - 42 St") %>%
#'  dplyr::select(stop_id)
#' # Note that multiple unrelated stops can have the same stop_name, see cluster_stops() for examples how to find these cases

#'departures(
#'  gtfs = gtfs,
#'  stop_ids = stop_ids,
#'  date_ymd = "2018-08-23",
#'  start_hms = lubridate::hms("7:00:00"),
#'  end_hms = lubridate::hms("7:10:00")
#')
