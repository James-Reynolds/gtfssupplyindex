#' Create stops_in_or_near_areas table
#' 
#' Calculate list of stops_in_or_near_areas tables (by route-type) from  list of tidygtfs (by route_type) 
#'
#' @param list_gtfs list of tidygtfs (split by route), output of gtfs_by_route_type.R
#' @param areas_of_interest sf with areas_of_interest, area_id and geometry 
#' @param buffer_distance datatable of route_type, buffer_distance and short_name, output of load_buffer_zone.R
#' @param EPSG_for_transform value for the CRS to transform from lat/lon to metres
#' @param verbose If TRUE, output of each step will be printed or plotted.  
#'
#' @return list of dataframes, containing the stops_in_or_near_areas table for each individual route_type
#' @export
#'
#' @examples
#' #load the revised mornington GTFS data
#' list_gtfs = gtfssupplyindex:::gtfs_by_route_type(system.file(
#' "extdata/mornington180109",
#' "gtfs.zip", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE))
#' 
#' areas_of_interest <- load_areas_of_interest(areas_of_interest = sf::st_read(system.file(
#' "extdata",
#' "mornington_sa12021.geojson", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE)), 
#' area_id_field = "sa1_code_2021")
#' 
#' buffer_distance <- gtfssupplyindex:::load_buffer_zones()
#' 
#' gtfssupplyindex:::stops_in_walk_dist(
#'  list_gtfs = list_gtfs, 
#'  areas_of_interest = areas_of_interest,
#'  EPSG_for_transform = 28355, 
#'  verbose = TRUE)
#'   
stops_in_walk_dist <- function(
    list_gtfs,
    areas_of_interest,
    EPSG_for_transform, 
    verbose = FALSE){

  # load buffer distances
  buffer_distance = gtfssupplyindex:::load_buffer_zones()
  if(verbose){head(buffer_distance) %>% knitr::kable(caption = "Step 1, load the buffer distances. This table shows the first 6 entries in the buffer_distance table")}
  
  # add buffer_distance_length to list (by route) of tidy_gtfs
  list_gtfs <- mapply(function(x, nm){ 
    append(x, 
           (buffer_distance_length = 
              buffer_distance[
                which(
                  buffer_distance$short_name %in% nm)
                , "buffer_distance"]))}, 
    list_gtfs, names(list_gtfs), SIMPLIFY=FALSE)
  
  if(verbose){
    names(list_gtfs[[1]]) %>% knitr::kable(caption = "Step 1, continued: This table shows the objects included in the 'rail' tidygtfs object, with the addition of a 'buffer_distance' object")
    list_gtfs[[1]]$buffer_distance %>% knitr::kable(caption = "Step 1, continued: This is the contents of the buffer_distance object, matching the 800m catchment zones of railway stations")
  }
  
  
  # transform areas_of_interest to CRS in metres
  areas_of_interest <- sf::st_transform(areas_of_interest, EPSG_for_transform)
 
  # calculate Area_area terms
  areas_of_interest$area_area <- sf::st_area(areas_of_interest)
  if(verbose){head(areas_of_interest %>% sf::st_drop_geometry()) %>% knitr::kable(caption = "Step 2, areas for each of the areas_of_interest")}
 
  # apply stops_in_walk_dist_one_route function to list (by route) of tidy_gtfs
  stops_in_or_near_areas <- lapply(list_gtfs, stops_in_walk_dist_one_route,
    areas_of_interest = areas_of_interest,
    EPSG_for_transform = EPSG_for_transform,
    verbose = verbose)
  return(stops_in_or_near_areas)
}



#' Calculate stops_in_or_near_areas table for a single route_type
#'
#' @param gtfs_single_route_type a tidygtfs with only one route_type AND the buffer_distance included as list element
#' @param areas_of_interest sf object with area_id field
#' @param EPSG_for_transform CRS to transform from lat/lon to metres
#'
#' @return dataframe with stop_id, area_id and area_terms for stop_in_or_near_areas
#' @export
#'
#' @examples
#' #load the revised mornington GTFS data
#' list_gtfs = gtfssupplyindex:::gtfs_by_route_type(system.file(
#' "extdata/mornington180109",
#' "gtfs.zip", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE))
#' 
#' areas_of_interest <- load_areas_of_interest(areas_of_interest = sf::st_read(system.file(
#' "extdata",
#' "mornington_sa12021.geojson", 
#' package = "gtfssupplyindex", 
#' mustWork = TRUE)), 
#' area_id_field = "sa1_code_2021")
#' 
#' buffer_distance <- gtfssupplyindex:::load_buffer_zones()
#' 
#' # add buffer_distance_length to list (by route) of tidy_gtfs
#' list_gtfs <- mapply(function(x, nm){ 
#' append(x, (buffer_distance_length = buffer_distance[which(
#' buffer_distance$short_name %in% nm), "buffer_distance"]))}, 
#' list_gtfs, names(list_gtfs), SIMPLIFY=FALSE)
#'  
#'  
#'  # transform areas_of_interest to CRS in metres
#'   EPSG_for_transform = 28355
#'  areas_of_interest <- sf::st_transform(areas_of_interest, EPSG_for_transform)
#'  
#'  # calculate Area_area terms
#'  areas_of_interest$area_area <- sf::st_area(areas_of_interest)
#'   
#' gtfssupplyindex:::stops_in_walk_dist_one_route(
#'  gtfs_single_route_type = list_gtfs[[1]], 
#'  areas_of_interest = areas_of_interest,
#'  EPSG_for_transform = 28355, 
#'  verbose = TRUE)
#'   
#' 
#' 
stops_in_walk_dist_one_route <- function(
    gtfs_single_route_type = list_gtfs[[1]],
    areas_of_interest,
    EPSG_for_transform,
    verbose = FALSE
    ){
  #get stops as sf from the gtfs, listed by route_type, using stops_as_sf_function (see below)
  stops_as_sf <- tidytransit::stops_as_sf(gtfs_single_route_type$stops)
  #drop everything except geometry and stop_id
  stops_as_sf <- stops_as_sf %>% dplyr::select(stop_id, geometry)
  
  # map the stops onto the areas_of_interest for the first element in the list
  #map + 
  #  ggplot2::geom_sf(data=list_of_stops_as_sf[[1]], size = 2)
  
  # transform stops to CRS in metres
  stops_as_sf <- stops_as_sf %>% sf::st_transform(crs = EPSG_for_transform)
  if(verbose){head(stops_as_sf) %>% knitr::kable(caption = "The list of stops extracted from the rail tidygtfs object and transformed into metres")}
  
  #draw radius around stops of the buffer zone with buffer zone distance set in metres
  circles_around_stops <- stops_as_sf %>% sf::st_buffer(dist = units::set_units(gtfs_single_route_type$buffer_distance,m))
 
  # Intersect the circles
  stops_in_or_near_areas <- sf::st_intersection(areas_of_interest, circles_around_stops)
  if(verbose){ 
    # plot only first 6 stops
    plot_area_bn <- ggplot2::ggplot() + 
        ggplot2::geom_sf(data=stops_in_or_near_areas, ggplot2::aes(fill = area_id)) + 
      ggplot2::theme(legend.position = "bottom", 
                     axis.text.x=ggplot2::element_blank(), #remove x axis labels
                     axis.ticks.x=ggplot2::element_blank(), #remove x axis ticks
                     axis.text.y=ggplot2::element_blank(),  #remove y axis labels
                     axis.ticks.y=ggplot2::element_blank()  #remove y axis ticks
                     ) 
    print(plot_area_bn)
  
    
    
    # plot for each stop, but only first six
    print(ggplot2::ggplot() + 
            ggplot2::geom_sf(data=stops_in_or_near_areas %>% 
                      dplyr::filter(stop_id %in%
                               (stops_in_or_near_areas$stop_id %>%
                                  unique() %>% head()))
                    , ggplot2::aes(fill = area_id)) + 
            ggplot2::theme(legend.position = "none", 
                           axis.text.x=ggplot2::element_blank(), #remove x axis labels
                           axis.ticks.x=ggplot2::element_blank(), #remove x axis ticks
                           axis.text.y=ggplot2::element_blank(),  #remove y axis labels
                           axis.ticks.y=ggplot2::element_blank()  #remove y axis ticks
                           ) +
            ggplot2::facet_wrap(ggplot2::vars(stop_id))
    )
 
  }
  
  
  # calculate the area_BN_term and drop geometry
  stops_in_or_near_areas$area_Bn <- sf::st_area(stops_in_or_near_areas)
  stops_in_or_near_areas <- stops_in_or_near_areas %>% sf::st_drop_geometry()

  
  
  # join areas_of_interest areas (but not the area geometry)
  stops_in_or_near_areas <- dplyr::left_join(stops_in_or_near_areas, areas_of_interest %>% sf::st_drop_geometry())
  
  # calculate combined area terms 
  stops_in_or_near_areas$area_terms <- stops_in_or_near_areas$area_Bn / stops_in_or_near_areas$area_area
  # drop units 
  stops_in_or_near_areas$area_terms <- as.numeric(stops_in_or_near_areas$area_terms)
  
  if(verbose){head(stops_in_or_near_areas %>% dplyr::select(stop_id, area_id, area_terms)) %>%
    knitr::kable(caption = "The calculated area_terms for each combination of area and stop")}
  
  return(stops_in_or_near_areas %>% dplyr::select(stop_id, area_id, area_terms))
  
}
  



