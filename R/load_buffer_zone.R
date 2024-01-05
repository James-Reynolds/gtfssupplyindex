
#' Loads buffer zones from user defined path or from default csv file
#'
#' @param buffer_zones_definition_path optional string defining path to a csv defining the buffer zones
#'
#' @return tibble containing route_type, buffer_distance and short_name
#' @export
#'
#' @examples
#' load_buffer_zones()
#' 
load_buffer_zones <- function(
    buffer_zones_definition_path = system.file(
      "extdata",
      "buffer_zones.csv", 
      package = "gtfssupplyindex", 
      mustWork = TRUE)) 
{

buffer_distance <- tibble::as_tibble(
  read.csv(buffer_zones_definition_path) %>%
    dplyr::select(route_type, buffer_distance, short_name)
)

return(buffer_distance)
}