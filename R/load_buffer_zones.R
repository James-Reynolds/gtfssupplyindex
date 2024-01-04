#' Load the GTFS Route Types and buffer zone information
#'
#' `load_buffer_zones ()` loads information from the buffer_zones.csv about route_types, their names and the associated walking distance thresholds.  
#'
#' @param buffer_zones_definition_path String describing the path to the buffer_zone definition file. Defaults to the buffer_zones.csv file included in the packages data directory.  
#' @return a tibble containing the buffer_distance table,  which has an entry for each route type defining its buffer_distance (typically 400 or 800m) and the short_name of each route type
#' @export
load_buffer_zones <- function(buffer_zones_definition_path = "inst/extdaya/buffer_zones.csv"){

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
