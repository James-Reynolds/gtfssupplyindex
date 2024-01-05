#' Build the areas_of_interest table
#'
#' @param areas_of_interest geojson with areas of interest. Defaults to NYC borough boundaries from mfherman/nycgeo package. 
#' @param area_id_field the primary key field in the areas_of_interest sf. Defaults to geoid, as per NYC borough boundaries
#'
#' @return areas_of_interest table
#' @export
#'
#' @examples
#' load_areas_of_interest()
#' 
load_areas_of_interest <- function(areas_of_interest = sf::st_read(system.file(
                                     "extdata",
                                     "nyc.geojson", 
                                     package = "gtfssupplyindex", 
                                     mustWork = TRUE)), 
                                   area_id_field = "geoid"){
  areas_of_interest <- areas_of_interest[,paste(area_id_field)]
  names(areas_of_interest) <- c("area_id", "geometry")
  return(areas_of_interest)
  
}
