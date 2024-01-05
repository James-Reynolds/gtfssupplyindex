#' tidygtfs object to list by route_type of tidygtfs object 
#'
#' `gtfs_by_route_type()` loads a GTFS into the datatable format used by gtfstool, then splits it by route_type, and returns a list with a tidygtfs for each route type
#'
#' @param path to GTFS .zip file
#'
#' @returns A list, each element being a tidygtfs for an individual mode.
#' @export
#' 
#' @examples
#' # Load sample google-supplied sample gtfs from gtfstools
#' path <- file.path(system.file("extdata", package = "gtfstools"), "ggl_gtfs.zip")
#' gtfs_by_route_type(path)
#'
#' # Load NYC MTA sample gtfs from tidytransit
#' path <- file.path(system.file("extdata", package = "tidytransit"), "google_transit_nyc_subway.zip")
#' gtfs_by_route_type(path)


gtfs_by_route_type <- function(path){ 
  
  #load gtfs as a gtfs_tools_object
  gtfs_as_gtfs_tools_object <- gtfstools::read_gtfs(path)
  
  buffer_distance <- gtfssupplyindex:::load_buffer_zones()
  # Keep only buffer distance definitions for those routes_types in the gtfs
  buffer_distance <- buffer_distance[ 
          which(
            buffer_distance$route_type %in% 
              gtfs_as_gtfs_tools_object$routes$route_type)
          ,]
 
  # create list by route_type 
  gtfs_by_route_type <- list()
  for(i in seq(1:nrow(buffer_distance))){
    gtfs_by_route_type[[length(gtfs_by_route_type)+1]] <- tidytransit::as_tidygtfs(
      gtfstools::filter_by_route_type(
        gtfs_as_gtfs_tools_object, 
        route_type = as.integer(
          buffer_distance[i,1])
      )
    )
    names(gtfs_by_route_type)[i] <- buffer_distance[i,3]
  }
  return(gtfs_by_route_type)
}

