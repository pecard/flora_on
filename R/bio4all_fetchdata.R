## Get Data from Inaturalist aggregated to the Flora-on Project ##

# Load packages ----
#remotes::install_github("ropensci/rinat")
kpacks <- c('rinat', 'tidyverse', 'sf', 'data.table', 'lubridate', 'viridis', 'raster',
            'ggtext')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# Load data Admin data
admin0_cont <- readRDS('./data/admin0_cont.rds')
admin1_cont <- readRDS('./data/admin1_cont.rds')

# Load utm grid
utm10 <- readRDS('./data/utm10.rds')

# Source functions
source('./R/f_ggtheme.R') # ggplot theme

# Get data from Flora-on ----
fon <- get_inat_obs_project('flora-on')

# Data.table ----
setDT(fon) # coerce to DT

## Adjust classes
fon[, created_at := ymd_hms(created_at) ] # Coerce to datetime
fon[, day := as.POSIXct(trunc(created_at, 'days')) ] # round day

setkey(fon, created_at) # DT key

# Records from the last 7 days ----
ini <- Sys.Date() - 7
end <- Sys.Date()

# Week summaries ----
fon[between(created_at, ini, end)][, .(count = .N), by = day]
fon[created_at %between% c(ini, end)][, .(count = .N)]

## Coerce to sf
fonweek <-
  fon[created_at %between% c(ini, end)][
    , c('id', 'latitude', 'longitude', 'observed_on')] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

## Aggregate observations by Level1 (District)
p_ag1 <-
  aggregate(fonweek, admin1_cont['NAME_1'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                 labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  )

## Aggregate observations by UTM10x10km
p_utm <-
  aggregate(fonweek, utm10['UTM'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))

# Plot UTM map with weekly observations
ggplot() +
  geom_sf(data = admin0_cont, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, data = p_utm) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(title = paste("Total de Regitos: ", sum(p_utm$id, na.rm = T)),
       subtitle = paste('Registos obtidos entre', ini, 'e', end),
       caption = 'Dados do projeto Flora-on - Biodiversity4all\n
       www.biodiversity4all.org/projects/flora-on',
       x = NULL, y = NULL) +
  theme_map()

