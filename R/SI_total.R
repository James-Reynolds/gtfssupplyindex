#' Total si_by_route_type lit (by route_type) of tables into a si_by_area table
#'
#'@description Having the si by route type may be useful, 
#'but so too is having the total for all modes. This function sums up the SIs
#'by mode to give a total for each area of interest
#'
#' @param si_by_route_type a list (by route_type) of areas and SI scores
#'
#' @return a datatable with areas and total SIs
#' @export
#'
#' @examples
#' si_total(si_by_route_type)
#' 
si_total <- function(si_by_route_type) {
  
si_by_area <- bind_rows(si_by_route_type, .id = "route_type")
si_by_area <- aggregate(SI ~ area_id, si_by_area, sum)


  return(si_by_area)
   
}