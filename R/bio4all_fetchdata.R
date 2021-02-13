## Get Data from Inaturalist aggregated with Flora-on Project ##

# Load packages ----
#remotes::install_github("ropensci/rinat")
pacman::p_load(rinat, tidyverse, data.table, raster, lubridate, sf, tmap)

# Get data from Flora-on ----
fon <- get_inat_obs_project('flora-on')

# Data.table ----
setDT(fon)
# Adjust classes
fon[, created_at := ymd_hms(created_at) ] # posixct
fon[, day := as.POSIXct(trunc(created_at, 'days')) ] # round day

setkey(fon, created_at) # DT key

# Summaries ----
fon[created_at > (Sys.Date() - 6)][, .(count = .N), by = day]
fon[created_at > (Sys.Date() - 6)][, .(count = .N)]

# records from the last 7 days ----
ini <- Sys.Date() - 6
end <- Sys.Date()

fonweek <-
  fon[created_at %between% c(ini, end)][
    , c('id', 'latitude', 'longitude', 'observed_on')] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")


# Aggregate observations by Level1 (District)
p_ag1 <-
  aggregate(fonweek, adm1_cont['NAME_1'],
            FUN = function(x) length(x))

p_utm <-
  aggregate(fonweekg, utm10['UTM'],
            FUN = function(x) length(x))

ggplot() +
  #geom_sf(data = adm1_cont, fill = NA, colour = 'grey20', alpha = .3) +
  geom_sf(aes(fill = id), color = NA, data = p_utm) +
  geom_sf(data = adm0_cont, fill = NA, color = 'grey80', size = .01) +
  scale_fill_viridis_c('Registos', option = "plasma") +
  labs(title = paste("Total de Regitos: ", sum(p_utm$id, na.rm = T)),
       x = NULL, y = NULL) +
  theme_void()


