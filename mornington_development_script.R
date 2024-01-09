
### ---------------- get abs data for Mornington Peninsula
###options(timeout = 1000)
####remotes::install_github("wfmackey/absmapsdata")

get_mornington_sa1 <- function(){
  mornington_sa12021 <- absmapsdata::sa12021 %>% filter(sa3_name_2021 == "Mornington Peninsula") %>% select(sa1_code_2021)
  sf::st_write(mornington_sa12021, "inst/extdata/mornington_sa12021.geojson", append=FALSE)
  return(mornington_sa12021)
}

get_mornington_sa3 <- function(){
  mornington_sa32021 <- absmapsdata::sa32021 %>% filter(sa3_name_2021 == "Mornington Peninsula") %>% select(sa3_code_2021)
  sf::st_write(mornington_sa32021, "inst/extdata/mornington_sa32021.geojson", append=FALSE)
  return(mornington_sa32021)
}




### ---------------- load SA3 abs maps data for just mornington peninsula
areas_of_interest <- load_areas_of_interest(areas_of_interest = sf::st_read(system.file(
  "extdata",
  "mornington_sa32021.geojson", 
  package = "gtfssupplyindex", 
  mustWork = TRUE)), 
  area_id_field = "sa3_code_2021")

### --OR-------------- load SA1 abs maps data for just mornington peninsula
areas_of_interest <- load_areas_of_interest(areas_of_interest = sf::st_read(system.file(
  "extdata",
  "mornington_sa12021.geojson", 
  package = "gtfssupplyindex", 
  mustWork = TRUE)), 
  area_id_field = "sa1_code_2021")


# map the areas_of_interest
map <- areas_of_interest %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry))
map
#set the EPSG to transform from lat/lon to metres
EPSG_for_transform = 28355

### ---------------------------- load and update the route_types in the mornington gtfs data
mornington_2019 <- tidytransit::read_gtfs("inst/extdata/mornington180109/gtfs.zip")
#deal with issue to do with missing route_color information
mornington_2019$routes$route_color <- "Blue"
#convert route type from 107 (tourist railway) to 2 (rail) ---NEW FEATURE NEEDED --- EXPAND HANDLING OF MODES TO ENABLE CALCULATION OF MODES FROM EXTENDED GTFS LIST
mornington_2019$routes$route_type <- as.integer(2)
# write back to zip file
tidytransit::write_gtfs(mornington_2019, "inst/extdata/mornington180109/gtfs.zip")

#load the revised mornington GTFS data
list_gtfs = gtfssupplyindex:::gtfs_by_route_type("inst/extdata/mornington180109/gtfs.zip")

stops_as_sf_mornington <-  list_gtfs[[1]]$stops %>% 
  tidytransit::stops_as_sf()

#map the stops on the ABS data
map + 
  geom_sf(data = stops_as_sf_mornington, aes(geometry = geometry))

#load buffer distance data
buffer_distance <- gtfssupplyindex:::load_buffer_zones()

stops_in_or_near_areas <- gtfssupplyindex:::stops_in_walk_dist(
  list_gtfs = list_gtfs, 
  areas_of_interest = areas_of_interest,
  buffer_distance = buffer_distance,
  EPSG_for_transform = 28355
)


