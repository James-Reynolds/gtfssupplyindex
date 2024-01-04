#' tidygtfs object to list by route_type of tidygtfs object 
#'
#' `gtfs_by_route_type()` loads a GTFS into the datatable format used by gtfstool, then splits it by route_type, and returns a list with a tidygtfs for each route type
#'
#' @param path to GTFS .zip file
#'
#' @returns A list, each element being a tidygtfs for an individual mode.
#'
#' @export
load_buffer_zones <- function(buffer_zones_definition_path = "inst/extdata/buffer_zones.csv"){
  
  buffer_zone_definitions <- tibble::as_tibble(
    read.csv(buffer_zones_definition_path)
  )
  
  class(buffer_zone_definitions$route_type) <- "character"
  buffer_zone_definitions$mode <- noquote(buffer_zone_definitions$mode)
  buffer_zone_definitions$description_examples <- noquote(buffer_zone_definitions$description_examples)
  class(buffer_zone_definitions$mode) <- "character"
  class(buffer_zone_definitions$description_examples) <- "character"
  #set buffer zone units to metres
  buffer_zone_definitions$buffer <- units::as_units(buffer_zone_definitions$buffer_distance, "m")
  
  return(buffer_zone_definitions %>% dplyr::select(route_type, buffer_distance, short_name)
  )
  
}





gtfs_by_route_type <- function(path){ 
  
  #load gtfs as a gtfs_tools_object
  gtfs_as_gtfs_tools_object <- gtfstools::read_gtfs(path)
  
  
  #load buffer_distances information for route_types in gtfs
  buffer_distance <- load_buffer_zones()
  buffer_distance <- buffer_distance %>% 
    dplyr::filter(
      route_type == as.character(
        unique(gtfs_as_gtfs_tools_object$routes$route_type)
        )
      )
  
  #gtfs_lrt <- as_tidygtfs(filter_by_route_type(gtfs_as_gtfs_tools_object, route_type = 0))
 # gtfs_subway <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 1))
 # gtfs_rail <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 2))
#  gtfs_bus <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 3))
#  gtfs_ferry <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 4))
 # gtfs_cable_tram <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 5))
#  gtfs_aerial_lift <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 6))
 # gtfs_funicular <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 7))
  #gtfs_trolleybus <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 11))
#  gtfs_monorail <- as_tidygtfs(filter_by_route_type(gtfs, route_type = 12))
  
  
  gtfs_by_route_type <- list(
    tidytransit::as_tidygtfs(gtfstools::filter_by_route_type(gtfs_as_gtfs_tools_object, route_type = as.integer(buffer_distance$route_type))
                )
  )
  names(gtfs_by_route_type) <- buffer_distance$short_name  
    
  return(gtfs_by_route_type)
}

#' @examples
# Lot sample google-supplied sample gtfs
path <- file.path(system.file("extdata", package = "gtfstools"), "ggl_gtfs.zip")
gtfs_by_route_type(path)


