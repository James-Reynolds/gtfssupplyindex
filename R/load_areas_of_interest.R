#' Build the areas_of_interest table
#'
#' @param areas_of_interest sf containing boundaries for which to calculate the SI.
#' Defaults to load mornington_sa3201 boundaries from extdata. 
#' @param area_id_field the primary key field in the boundaries sf. 
#' Defaults to sa3_code_2021 as per mornington dataset
#'
#' @return areas_of_interest table as an sf object, 
#' containing only area_id and geometry fields 
#' @export
#'
#' @examples
#' load_areas_of_interest()
#' 
load_areas_of_interest <- function(areas_of_interest = sf::st_read(system.file(
                                     "extdata",
                                     "mornington_sa32021.geojson", 
                                     package = "gtfssupplyindex", 
                                     mustWork = TRUE)), 
                                   area_id_field = "sa3_code_2021"){
  areas_of_interest <- areas_of_interest[,paste(area_id_field)]
  names(areas_of_interest) <- c("area_id", "geometry")
  return(areas_of_interest)
  
}
