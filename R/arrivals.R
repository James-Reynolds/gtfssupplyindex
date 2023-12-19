#' Determine the number of arrivals for a single mode at a stop between two time and dates
#'
#' @param gtfs_feed A tidygtfs containing the timetable information
#' @param stop_id A character string containing the id of the stop
#' @param start_ymd_hms A character string describing the start date and time of the period of interest, timezone local to the gtfs files.
#' @param end_ymd_hms A character string describing the end date and time of the period of interest, timezone local to the gtfs files.
#' @param route_type Numeric describing the type of transportation the number of arrivals is to be calculated for, based on the GTFS static definitions
#'
#' @return
#' @export
#'
#' @examples
arrivals <- function(gtfs_feed = gtfs_feed, stop_id = stop_id, start_ymd_hms = start_ymd_hms, end_ymd_hms = end_ymd_hms, route_type) {


}
