#------Function to run over the list (by route_type) or tidygtfs objects
SI_calc <- function(
    list_gtfs,
    stops_in_or_near_areas, 
    date_ymd, 
    start_hms,
    end_hms,
    verbose = FALSE){
  
  
  
  # add route_type names to list (by route) of tidy_gtfs
  stops_in_or_near_areas_list_with_route_name <- mapply(function(x, nm){ 
    route_type_name = nm
    x <- list(x)
    append(x, 
           route_type_name)
                }, 
    stops_in_or_near_areas, names(stops_in_or_near_areas), SIMPLIFY=FALSE)
  
  
  # apply SI_calc_one_route function to list (by route) of stops_in_or_near_areas
  si_by_mode_and_time <- lapply(stops_in_or_near_areas_list_with_route_name, SI_calc_one_route,
                                  list_gtfs,
                                  date_ymd,
                                  start_hms,
                                  end_hms,
                                  verbose)
  
  return(si_by_mode_and_time)
}



#-----SI calc Function for an individual moode (route_type)

SI_calc_one_route <- function(
    stops_in_or_near_areas_list_with_route_name_one_route_type, 
    list_gtfs,
    date_ymd, 
    start_hms,
    end_hms,
    verbose = FALSE){

  si_by_mode_and_time_one_route_type <- left_join(stops_in_or_near_areas_list_with_route_name_one_route_type[[1]], gtfs$stops %>% select(stop_id, stop_lon), by = "stop_id")

return(si_by_mode_and_time_one_route_type)  
  
}