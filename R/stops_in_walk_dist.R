#' Determine which stops are within walking distance of which areas of intersest
#'
#' @param list_gtfs A list, each element being a tidygtfs for an individual mode.
#' @param areas_of_interest sf object with area_id field and geometry only
#' @param buffer_distance buffer distance definitions 
#'
#' @return a stops_in_or_near_areas tibble consisting of the stop_id, area_id and route_type details. 
#' The area_terms are calculated latedin the area_terms function.   
#' @export
#'
#' @examples
#' stops_in_walk_dist()
stops_in_walk_dist <- function(
    list_gtfs = gtfssupplyindex:::gtfs_by_route_type(
      path = file.path(
        system.file(
          "extdata", 
          package = "tidytransit"),
        "google_transit_nyc_subway.zip")
      ),
    areas_of_interest = gtfssupplyindex:::load_areas_of_interest(
      areas_of_interest = sf::st_read(
        system.file(
      "extdata",
      "nyc.geojson", 
      package = "gtfssupplyindex", 
      mustWork = TRUE)), 
      area_id_field = "geoid"),
    buffer_distance = gtfssupplyindex:::load_buffer_zones()
    ){

  # Keep only buffer distance definitions for those routes_types in the gtfs
  buffer_distance <- buffer_distance[ 
    which(
      buffer_distance$short_name %in% 
    names(list_gtfs))
    ,]
  
  # Obtain CRS of geojson
  ## STILL NEED TO UNDERSTAND HOW TO MAKE THIS BE IN METRES RATHER THAN FT FOR NYC
  areas_of_interest_crs <- st_crs(areas_of_interest)$input
  
  # Convert the lat and long in the gtfs into x and y to match the geojson
  stops_x_y <- tibble(stop_id = 
                        list_gtfs$subway$stops$stop_id,
                      stop_x =   list_gtfs$subway$stops$stop_lon,
                      stop_y =   list_gtfs$subway$stops$stop_lat
  )
  
  
  
}