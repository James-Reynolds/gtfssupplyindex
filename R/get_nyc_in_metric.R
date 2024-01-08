#' Function for creating the nyc example dataset
#' 
#' Loads from the nycgeo package and converts to metres. Saves to the inst/extdata directory
#'
#' @return the nyc boroughs as a sf in metres (crs 32118)
#' @export
#'
#' @examples
#' get_nyc_in_metric
get_nyc_in_metric <- function(){
  nyc_boroughs_sf_in_feet <- nycgeo::borough_sf
  nyc_boroughs_sf_in_metres <- sf::st_transform(nyc_boroughs_sf_in_feet, 32118)
  sf::st_write(nyc_boroughs_sf_in_metres, "inst/extdata/nyc.geojson", append=FALSE)
  return(nyc_boroughs_sf_in_metres)
  }