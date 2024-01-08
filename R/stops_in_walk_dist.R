#' Determine which stops are within walking distance of which areas of intersest
#'
#' @param gtfs_single_route_type A list, each element being a tidygtfs for an individual mode.
#' @param areas_of_interest sf object with area_id field and geometry only
#' @param buffer_distance buffer distance definitions 
#'
#' @return a stops_in_or_near_areas tibble consisting of the stop_id, area_id and route_type details. 
#' The area_terms are calculated latedin the area_terms function.   
#' @export
#'
#' @examples
#' stops_in_walk_dist()
#' 


stops_in_walk_dist <- function(
    gtfs_single_route_type = list_gtfs[[1]],
    areas_of_interest = areas_of_interest,
    buffer_distance = gtfssupplyindex:::load_buffer_zones(),
    EPSG_for_transform = EPSG_for_transform 
    ){

  # Keep only buffer distance definitions for those routes_types in the gtfs
  buffer_distance <- buffer_distance[ 
    which(
      buffer_distance$short_name %in% 
    names(gtfs_single_route_type))
    ,]
 
  #get stops as sf from the gtfs, listed by route_type, using stops_as_sf_function (see below)
  list_of_stops_as_sf <- lapply(gtfs_single_route_type, stops_as_sf_function)
  #drop everything except geometry and stop_id
  list_of_stops_as_sf <- lapply(list_of_stops_as_sf, dplyr::select, stop_id, geometry)
  
  # map the stops onto the areas_of_interest for the first element in the list
  #map + 
  #  geom_sf(data=list_of_stops_as_sf[[1]], size = 2)
  
  # transform stops to CRS in metres
  list_of_stops_as_sf <- lapply(list_of_stops_as_sf, st_transform, EPSG_for_transform)
  
  #draw radius around stops of the buffer zone
  list_of_circles_around_stops <- lapply(list_of_stops_as_sf, st_buffer, dist = buffer_distance$buffer_distance)
  
  # transform areas_of_interest to CRS in metres
  areas_of_interest <- st_transform(areas_of_interest, EPSG_for_transform)


  # calculate Area_area terms
  areas_of_interest$area_area <- st_area(areas_of_interest)
 
  
  # Intersect the circles
  stops_in_or_near_areas <- st_intersection(areas_of_interest, dat_circles)
  # calculate the area_BN_term and drop geometry
  stops_in_or_near_areas$area_Bn <- st_area(stops_in_or_near_areas)
  stops_in_or_near_areas <- stops_in_or_near_areas %>% st_drop_geometry()
  
  # drop areas_of_interest geometry
  areas_of_interest <- areas_of_interest %>% st_drop_geometry()
  
  # join areas_of_interest areas
  stops_in_or_near_areas <- left_join(stops_in_or_near_areas, areas_of_interest)
  
  # calculate combined area terms 
  stops_in_or_near_areas$area_terms <- stops_in_or_near_areas$area_Bn / stops_in_or_near_areas$area_area
  # drop units 
  stops_in_or_near_areas$area_terms <- as.numeric(stops_in_or_near_areas$area_terms)
  
  return(stops_in_or_near_areas %>% select(stop_id, area_id, area_terms))
  
}
  

### Function to get stops as sf from the gtfs, listed by route_type
stops_as_sf_function <- function(gtfs){ 
  stops_as_sf <- tidytransit::stops_as_sf(gtfs$stops)
  return(stops_as_sf)
}

