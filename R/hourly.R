#' Calculate SI for each hour (of service) on a given date, by area_of_interest
#'
#' @param list_gtfs Output of the gtfs_by_route_type function, 
#' a list (by route_type) of tidygtfs objects
#' @param stops_in_or_near_areas Output of the stops_in_walk_dist function. 
#' A list (by route_type) of stops and areas within walking distance of each other 
#' and the associated area_terms of SI calculation
#' @param date_ymd The date of interest
#' @param verbose Print details of progress during calculations
#'
#' @return A dataframe with SI for each combination of area and hour of service
#' @export
#'
#' @examples
#' 
#' ####-----first load all the inputs
#' #load the revised mornington GTFS data
#' list_gtfs = gtfssupplyindex:::gtfs_by_route_type(system.file(
#' "extdata/mornington180109",
#' "gtfs.zip", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE))
#' 
#' areas_of_interest <- load_areas_of_interest(absmapsdata::sa22021 %>% 
#'             dplyr::filter(sa3_name_2021 ==  "Mornington Peninsula") %>% 
#'                                              dplyr::select(sa2_code_2021),  
#'                                            area_id_field = "sa2_code_2021") 
#' buffer_distance <- gtfssupplyindex:::load_buffer_zones()
#' 
#' stops_in_or_near_areas <- gtfssupplyindex:::stops_in_walk_dist(
#' list_gtfs = list_gtfs, 
#' areas_of_interest = areas_of_interest,
#' EPSG_for_transform = 28355,
#' verbose = FALSE
#' )
#' 
#' hourly(list_gtfs, stops_in_or_near_areas, "2018-12-30")

hourly <- function(
    list_gtfs, 
    stops_in_or_near_areas,
    date_ymd, 
    verbose = FALSE)
{
  
#find first and last arrival times across the list of gtfs  
  first_arrival <- lapply(list_gtfs, earliest_arrival) %>% as.data.frame() 
  first_arrival <- do.call(pmin, first_arrival)
  last_arrival <- lapply(list_gtfs, latest_arrival) %>% as.data.frame() 
  last_arrival <- do.call(pmax, last_arrival)
  
#convert into first and last hour (ie. 10, 16) as string
  first_hour <- lubridate::hour(first_arrival)
  last_hour <- lubridate::hour(last_arrival)
  
  ms <- "00:00"  
 
# create table for output  
si_by_area_and_hour <- data.frame()
 
  
# loop SI calculation over hour periods between first and last hour
for(i in seq(first_hour, last_hour)){
  start_hms <- lubridate::hms(paste(i, ms))
  end_hms <- start_hms + lubridate::hms("1:00:00")
  if(verbose){print(paste("Now calculating", start_hms, "to", end_hms))}
  si_by_route_type <- gtfssupplyindex::si_calc(
    list_gtfs = list_gtfs,
    stops_in_or_near_areas = stops_in_or_near_areas, 
    date_ymd = date_ymd, 
    start_hms = start_hms,
    end_hms = end_hms,
    verbose = verbose)

  si_by_area <- si_total(si_by_route_type)
  si_by_area$hour_starting <- paste(as.character(lubridate::hour(start_hms)), ":00", sep = "")

  si_by_area_and_hour <- rbind(si_by_area_and_hour, si_by_area)  

}


return(si_by_area_and_hour)

}



#' Returns the time of the earliest arrival in a tidygtfs object
#'
#' @param gtfs a tidygtfs
#'
#' @return a lubridate time
#' @export
#'
#' @examples
#' 
#' #load the revised mornington GTFS data
#' gtfs = tidytransit::read_gtfs(system.file(
#' "extdata/mornington180109",
#' "gtfs.zip", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE))
#' 
#' earliest_arrival(gtfs)
#' 
#' 
earliest_arrival <- function(gtfs){
  return(gtfs$stop_times$arrival_time %>% as.character() %>% min() %>% lubridate::hms())
}



#' Returns the time of the latest arrival in a tidygtfs object
#'
#' @param gtfs a tidygtfs
#'
#' @return a lubridate time
#' @export
#'
#' @examples
#' 
#' #load the revised mornington GTFS data
#' gtfs = tidytransit::read_gtfs(system.file(
#' "extdata/mornington180109",
#' "gtfs.zip", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE))
#' 
#' latest_arrival(gtfs)
#' 
#' 
latest_arrival <- function(gtfs){
  return(gtfs$stop_times$arrival_time %>% as.character() %>% max() %>% lubridate::hms())
    }

