
### ---- load mornington test data
mornington_2019 <- read_gtfs("inst/extdata/mornington180109/gtfs.zip")
#deal with issue to do with missing route_color information
mornington_2019$routes$route_color <- "Blue"
#convert route type from 107 (tourist railway) to 2 (rail) ---NEW FEATURE NEEDED --- EXPAND HANDLING OF MODES TO ENABLE CALCULATION OF MODES FROM EXTENDED GTFS LIST
mornington_2019$routes$route_type <- as.integer(2)
# write back to zip file
write_gtfs(mornington_2019, "inst/extdata/mornington180109/gtfs.zip")

#load the mornington GTFS data
list_gtfs = gtfssupplyindex:::gtfs_by_route_type("inst/extdata/mornington180109/gtfs.zip")

### --- load abs maps data for just mornington penisula
###options(timeout = 1000)
####remotes::install_github("wfmackey/absmapsdata")
library(tidyverse)
library(sf)
library(absmapsdata)

#get areas of interest from ABS
areas_of_interest <- sa12021 %>% filter(sa3_name_2021 == "Mornington Peninsula") %>% select(sa1_code_2021)
names(areas_of_interest) <- c("area_id", "geometry")
#map the areas_of_interest
map <- areas_of_interest %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry))
map


#set the EPSG to transform from lat/lon to metres
EPSG_for_transform = 28355