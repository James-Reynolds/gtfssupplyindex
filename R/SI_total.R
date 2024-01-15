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
#' #'#load the revised mornington GTFS data
#'library(dplyr)
#'list_gtfs = gtfssupplyindex:::gtfs_by_route_type(system.file(
#'  "extdata/mornington180109",
#'  "gtfs.zip", 
#'  package = "gtfssupplyindex", 
#'  mustWork = TRUE))
#'
#'areas_of_interest <- load_areas_of_interest(absmapsdata::sa22021 %>% 
#'             filter(sa3_name_2021 ==  "Mornington Peninsula") %>% 
#'                                              select(sa2_code_2021),  
#'                                            area_id_field = "sa2_code_2021")
#'
#'buffer_distance <- gtfssupplyindex:::load_buffer_zones()
#'
#'stops_in_or_near_areas <- gtfssupplyindex:::stops_in_walk_dist(
#'  list_gtfs = list_gtfs, 
#'  areas_of_interest = areas_of_interest,
#'  EPSG_for_transform = 28355,
#'  verbose = FALSE
#')
#'
#'####----run SI.calc function to build si_by_mode_and_time list (by route_type) of tables
#'
#'si_by_route_type <- si_calc(
#'  list_gtfs = list_gtfs,
#'  stops_in_or_near_areas = stops_in_or_near_areas, 
#'  date_ymd = lubridate::ymd("2018-12-30"), 
#'  start_hms = lubridate::hms("10:30:00"),
#'  end_hms = lubridate::hms("16:00:00"),
#'  verbose = TRUE)
#' 
#' si_total(si_by_route_type)
#' 
si_total <- function(si_by_route_type) {
  
si_by_area <- dplyr::bind_rows(si_by_route_type, .id = "route_type")
si_by_area <- stats::aggregate(SI ~ area_id, si_by_area, sum)


  return(si_by_area)
   
}