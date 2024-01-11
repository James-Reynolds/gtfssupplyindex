#' Determine the number of departures from a vector of stop ids on a specific date between two times
#' 
#' This function is a modification of the example shown in https://r-transit.github.io/tidytransit/articles/timetable.html 
#'
#' @param gtfs A tidygtfs object containing the timetable information.
#' @param stop_ids A vector of strings with the stop id numbers from the gtfs.
#' @param date_ymd A character string describing the date of interest in yms format.
#' @param start_hms A character string describing the start time of the period of interest, timezone local to the gtfs. Services departing precisely at the start time are included
#' @param end_hms A character string describing the end time of the period of interest, timezone local to the gtfs. Services departing precisely at the end time are included
#'
#' @return A dataframe listing the number of departures for each stop_id in the specified time frame.
#'
#' @examples
#'
#'
#' @export

departures <- function(gtfs = gtfs,
                       stop_ids = stop_ids,
                       date_ymd = date_ymd,
                       start_hms = lubridate::hms("-1:0:0"),
                       end_hms = lubridate::hms("48:00:00")) {
  
  #----add the first_and_last_stops to the gtfs$trips table using function (below)
  gtfs <- gtfssupplyindex::first_and_last_stops(gtfs)
  
  # get stop_times and trip information
  departures <- stop_ids %>% 
    inner_join(gtfs$stop_times %>% 
                 select(trip_id, arrival_time, 
                        departure_time, stop_id, stop_sequence), 
               by = "stop_id")
  
  departures <- departures %>% 
    left_join(gtfs$trips %>% 
                select(trip_id, route_id, 
                       service_id, trip_headsign, 
                       trip_origin, trip_origin_id, first_stop_sequence_value, trip_destination, trip_destination_id, last_stop_sequence_value), 
              by = "trip_id") 
  
  # remove rows where the stop sequence is equal to the last stop, as these services terminate
  departures <- departures %>% filter(stop_sequence != last_stop_sequence_value)
  
  #  departures %>% filter(trip_id == 47) %>% select(stop_id, stop_sequence, departure_time, trip_destination, trip_destination_id, last_stop_sequence_value)
  
  ####----add route info (route_short_name) 
  # Each trip belongs to a route, and the route short name can be added to the departures by joining the trips data frame with gtfs$routes.
  
  departures <- departures %>% 
    left_join(gtfs$routes %>% 
                select(route_id, 
                       route_short_name), 
              by = "route_id")
  
  # Extract a single day-----------------This is again from the tidytransit article
  # Now we are ready to extract the same service table for any given day of the year.
  
  services_on_date <- gtfs$.$dates_services %>%
    dplyr::filter(date == date_ymd) %>%
    dplyr::select(service_id)
  
  departures_on_date <- departures %>%
    dplyr::inner_join(services_on_date, by = "service_id")
  
  #####----Here is where the function departs from what is shown in the tidytransit article about creating a departure timetable.----------------------------
  
  # get list of stop_id
  departures_by_stop_id <- data.frame(stop_id = stop_ids)
  
  # get number of departures for each stop_id
  departures_by_stop_id$departures <- apply(
    departures_by_stop_id,
    #rows only
    1, 
    FUN = function(x) nrow(
      departures_on_date %>% 
        dplyr::filter(stop_id == x[1]) %>% 
        dplyr::filter(departure_time >= start_hms) %>% 
        dplyr::filter(departure_time < end_hms)))
  
  return(departures_by_stop_id)
  
}


#' Determine the number of arrivals from a vector of stop ids on a specific date between two times
#' 
#' This function is a modification of the example shown in https://r-transit.github.io/tidytransit/articles/timetable.html 
#'
#' @param gtfs A tidygtfs object containing the timetable information.
#' @param stop_ids A vector of strings with the stop id numbers from the gtfs.
#' @param date_ymd A character string describing the date of interest in yms format.
#' @param start_hms A character string describing the start time of the period of interest, timezone local to the gtfs. Services departing precisely at the start time are included
#' @param end_hms A character string describing the end time of the period of interest, timezone local to the gtfs. Services departing precisely at the end time are included
#'
#' @return A dataframe listing the number of arrivals for each stop_id in the specified time frame.
#'
#' @examples
#' 
#'
#' @export

arrivals <- function(gtfs = gtfs,
                     stop_ids = stop_ids,
                     date_ymd = date_ymd,
                     start_hms = lubridate::hms("-1:0:0"),
                     end_hms = lubridate::hms("48:00:00")) {
  
  #----add the first_and_last_stops to the gtfs$trips table using function (below)
  gtfs <- gtfssupplyindex::first_and_last_stops(gtfs)
  
  # get stop_times and trip information
  arrivals <- stop_ids %>% 
    inner_join(gtfs$stop_times %>% 
                 select(trip_id, arrival_time, 
                        departure_time, stop_id, stop_sequence), 
               by = "stop_id")
  
  arrivals <- arrivals %>% 
    left_join(gtfs$trips %>% 
                select(trip_id, route_id, 
                       service_id, trip_headsign, 
                       trip_origin, trip_origin_id, first_stop_sequence_value, trip_destination, trip_destination_id, last_stop_sequence_value), 
              by = "trip_id") 
  
  # remove rows where the stop sequence is equal to the last stop, as these services terminate
  arrivals <- arrivals %>% filter(stop_sequence != first_stop_sequence_value)
  
  #  arrivals %>% filter(trip_id == 47) %>% select(stop_id, stop_sequence, departure_time, trip_destination, trip_destination_id, last_stop_sequence_value)
  
  ####----add route info (route_short_name) 
  # Each trip belongs to a route, and the route short name can be added to the arrivals by joining the trips data frame with gtfs$routes.
  
  arrivals <- arrivals %>% 
    left_join(gtfs$routes %>% 
                select(route_id, 
                       route_short_name), 
              by = "route_id")
  
  # Extract a single day-----------------This is again from the tidytransit article
  #Now we are ready to extract the same service table for any given day of the year.
  
  services_on_date <- gtfs$.$dates_services %>%
    dplyr::filter(date == date_ymd) %>%
    dplyr::select(service_id)
  
  arrivals_on_date <- arrivals %>%
    dplyr::inner_join(services_on_date, by = "service_id")
  
  ##--Here is where the function departs from what is shown in the tidytransit article about creating a departure timetable ---------------------
  
  # get list of stop_id
  arrivals_by_stop_id <- data.frame(stop_id = stop_ids)
  
  # get number of arrivals for each stop_id
  arrivals_by_stop_id$arrivals <- apply(
    arrivals_by_stop_id,
    #rows only
    1, 
    FUN = function(x) nrow(
      arrivals_on_date %>% 
        dplyr::filter(stop_id == x[1]) %>% 
        dplyr::filter(departure_time >= start_hms) %>% 
        dplyr::filter(departure_time < end_hms)))
  
  return(arrivals_by_stop_id)
  
}



#' Identify first and last stops of each trip
#' 
#' Function to identify the first and last stops (id, name and stop_sequence) 
#' for each route in a tidygtfs object
#'
#' @param gtfs a tidygtfs object
#'
#' @return a tidygtfs object with additional fields in the trip table for 
#' trip_origin, trip_orgin_id, first_stop_sequence_value, trip_destination, 
#' trip_destination_id and last_stop_sequence_value
#' @export
#'
#' @examples
first_and_last_stops <- function(gtfs = gtfs) {
  # This adapted from the tidytransit vignette on producing a departure timetable at https://r-transit.github.io/tidytransit/articles/timetable.html
  
  
  ##----trip_origin and trip_headsign----#
  # get the id of the first stop in the trip's stop sequence
  first_stop_id <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    summarise(stop_id = stop_id[which.min(stop_sequence)])
  
  # get the sequence number of the first stop in the trip's stop sequence 
  #(will likely be 1)
  first_stop_sequence_value <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    summarise(min(stop_sequence))
  
  first_stop_id <- left_join(first_stop_id, first_stop_sequence_value, by = "trip_id")
  names(first_stop_id) <- c("trip_id", "stop_id", "first_stop_sequence_value")
  
  # join with the stops table to get the stop_name
  first_stop_names <- left_join(first_stop_id, gtfs$stops, by="stop_id")
  
  # rename the first stop_name as trip_origin and the first_stop as trip_origin_id
  trip_origins <- first_stop_names %>% select(trip_id, trip_origin = stop_name, trip_origin_id = stop_id, first_stop_sequence_value)
  
  # join the trip origins back onto the trips
  gtfs$trips <- left_join(gtfs$trips, trip_origins, by = "trip_id")
  
  gtfs$trips %>%
    select(route_id, trip_origin, trip_origin_id, first_stop_sequence_value) %>%
    head()
  
  
  #In case trip_headsign does not exist in the feed it can be generated similarly to trip_origin:  
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
  
  
  ## -----trip_destination ----#  
  
  # get the id of the last stop in the trip's stop sequence
  last_stop_id <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    summarise(stop_id = stop_id[which.max(stop_sequence)])
  
  # get the sequence number of the last stop in the trip's stop sequence 
  #(will likely be 1)
  last_stop_sequence_value <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    summarise(max(stop_sequence))
  
  last_stop_id <- left_join(last_stop_id, last_stop_sequence_value, by = "trip_id")
  names(last_stop_id) <- c("trip_id", "stop_id", "last_stop_sequence_value")
  
  # join with the stops table to get the stop_name
  last_stop_names <- left_join(last_stop_id, gtfs$stops, by="stop_id")
  
  # rename the last stop_name as trip_origin and the last_stop as trip_origin_id
  trip_destinations <- last_stop_names %>% select(trip_id, trip_destination = stop_name, trip_destination_id = stop_id, last_stop_sequence_value)
  
  # join the trip origins back onto the trips
  gtfs$trips <- left_join(gtfs$trips, trip_destinations, by = "trip_id")
  
  gtfs$trips %>%
    select(route_id, trip_origin_id, trip_destination_id, trip_destination, last_stop_sequence_value) %>%
    head()
  
  return(gtfs)
}

