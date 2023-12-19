test_that("tidytransit NYC departures", {
  # Read  test data------------------------------------
  # We use a feed from the New York Metropolitan Transportation Authority. It is provided as a sample feed with tidytransit but you can read it directly from the MTA’s website.
  local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
  gtfs <- read_gtfs(local_gtfs_path)

  # To create a departure timetable, we first need to find the ids of all stops in the stops table with the same same name, as stop_name might cover different platforms and thus have multiple stop_ids in the stops table.
  stop_ids <- gtfs$stops %>%
    dplyr::filter(stop_name == "Times Sq - 42 St") %>%
    dplyr::select(stop_id)
  # Note that multiple unrelated stops can have the same stop_name, see cluster_stops() for examples how to find these cases

  date_as_ymd <- "2018-08-23"
  start_hms <- lubridate::hms("7:00:00")
  end_hms <- lubridate::hms("7:10:00")


  expect_equal(departures(
    gtfs = gtfs,
    stop_ids = stop_ids,
    date_as_ymd = date_as_ymd,
    start_as_hms = start_hms,
    end_as_hms = end_hms
  ), 28)
})
