
hourly <- function(
    list_gtfs, 
    stops_in_or_near_areas,
    date_ymd)
{
  
#find first and last arrival times across the list of gtfs  
  first_arrival <- lapply(list_gtfs, earliest_arrival) %>% as.data.frame() 
  first_arrival <- do.call(pmin, first_arrival)
  last_arrival <- lapply(list_gtfs, latest_arrival) %>% as.data.frame() 
  last_arrival <- do.call(pmax, last_arrival)
  
  first_hour <- lubridate::hour(first_arrival)
  last_hour <- lubridate::hour(last_arrival)
  
  
  
  
  si_by_route_type <- gtfssupply_index::si_calc(
    list_gtfs = list_gtfs,
    stops_in_or_near_areas = stops_in_or_near_areas, 
    date_ymd = date_ymd, 
    start_hms = lubridate::hms("10:30:00"),
    end_hms = lubridate::hms("16:00:00"),
    verbose = TRUE)
  
  
  
  
}



earliest_arrival <- function(gtfs){
  return(min(gtfs$stop_times$arrival_time) %>% seconds_to_period())
}



latest_arrival <- function(gtfs){
  return(max(gtfs$stop_times$arrival_time) %>% seconds_to_period())
}

