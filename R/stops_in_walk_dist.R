#' Create stops_in_or_near_areas table
#' 
#' Calculate list of stops_in_or_near_areas tables (by route-type) from  list of tidygtfs (by route_type) 
#'
#' @param list_gtfs list of tidygtfs (split by route), output of gtfs_by_route_type.R
#' @param areas_of_interest sf with areas_of_interest, area_id and geometry 
#' @param buffer_distance datatable of route_type, buffer_distance and short_name, output of load_buffer_zone.R
#' @param EPSG_for_transform value for the CRS to transform from lat/lon to metres
#'
#' @return list of dataframes, containing the stops_in_or_near_areas table for each individual route_type
#' @export
#'
#' @examples
stops_in_walk_dist <- function(
    list_gtfs,
    areas_of_interest,
    buffer_distance = gtfssupplyindex:::load_buffer_zones(),
    EPSG_for_transform){

  # Keep only buffer distance definitions for those routes_types in the gtfs
  buffer_distance <- buffer_distance[ 
    which(
      buffer_distance$short_name %in% 
        names(list_gtfs))
    ,]
  
  # transform areas_of_interest to CRS in metres
  areas_of_interest <- sf::st_transform(areas_of_interest, EPSG_for_transform)
 
  # calculate Area_area terms
  areas_of_interest$area_area <- sf::st_area(areas_of_interest)
 
  # apply stops_in_walk_dist_one_route function to list (by route) of tidy_gtfs
  stops_in_or_near_areas <- lapply(list_gtfs, stops_in_walk_dist_one_route,
    areas_of_interest = areas_of_interest,
    buffer_distance_length = as.numeric(
      buffer_distance[ 
        which(
          buffer_distance$short_name %in% 
            names(list_gtfs))
        , "buffer_distance"]
    ),
    EPSG_for_transform = EPSG_for_transform)
  return(stops_in_or_near_areas)
}


#' Calculate stops_in_or_near_areas table for a single route_type
#'
#' @param gtfs_single_route_type a tidygtfs with only one route_type 
#' @param areas_of_interest sf object with area_id field
#' @param buffer_distance_length walking distance in metres for the route_type
#' @param EPSG_for_transform CRS to transform from lat/lon to metres
#'
#' @return dataframe with stop_id, area_id and area_terms for stop_in_or_near_areas
#' @export
#'
#' @examples
stops_in_walk_dist_one_route <- function(
    gtfs_single_route_type = list_gtfs[[1]],
    areas_of_interest,
    buffer_distance_length = 800,
    EPSG_for_transform 
    ){
  #get stops as sf from the gtfs, listed by route_type, using stops_as_sf_function (see below)
  stops_as_sf <- tidytransit::stops_as_sf(gtfs_single_route_type$stops)
  #drop everything except geometry and stop_id
  stops_as_sf <- stops_as_sf %>% dplyr::select(stop_id, geometry)
  
  # map the stops onto the areas_of_interest for the first element in the list
  #map + 
  #  geom_sf(data=list_of_stops_as_sf[[1]], size = 2)
  
  # transform stops to CRS in metres
  stops_as_sf <- stops_as_sf %>% sf::st_transform(crs = EPSG_for_transform)
  
  #draw radius around stops of the buffer zone
  circles_around_stops <- stops_as_sf %>% sf::st_buffer(dist = buffer_distance_length)
 
  # Intersect the circles
  stops_in_or_near_areas <- sf::st_intersection(areas_of_interest, circles_around_stops)
  # calculate the area_BN_term and drop geometry
  stops_in_or_near_areas$area_Bn <- sf::st_area(stops_in_or_near_areas)
  stops_in_or_near_areas <- stops_in_or_near_areas %>% sf::st_drop_geometry()

  # join areas_of_interest areas (but not the area geometry)
  stops_in_or_near_areas <- dplyr::left_join(stops_in_or_near_areas, areas_of_interest %>% sf::st_drop_geometry())
  
  # calculate combined area terms 
  stops_in_or_near_areas$area_terms <- stops_in_or_near_areas$area_Bn / stops_in_or_near_areas$area_area
  # drop units 
  stops_in_or_near_areas$area_terms <- as.numeric(stops_in_or_near_areas$area_terms)
  
  return(stops_in_or_near_areas %>% dplyr::select(stop_id, area_id, area_terms))
  
}
  






