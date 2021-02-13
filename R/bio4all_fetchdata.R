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
end <- Sys.Date()
ini <- Sys.Date() - 6

fonweek <-
  fon[created_at %between% c(ini, end)][
    , c('id', 'latitude', 'longitude', 'observed_on')]

# Coerce to sf spatial ----
fonweekg <-
  fonweek %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")


# Aggregate observations by Level1 (District)
p_ag1 <-
  aggregate(fonweekg, adm1_cont['NAME_1'],
            FUN = function(x) length(x))

p_utm <-
  aggregate(fonweekg, utm10['UTM'],
            FUN = function(x) length(x))

# Plot with tmap ----
tmap_mode("plot")

p1 <- tm_shape(p_utm) +
  tm_fill(col = 'id',
          palette =  "viridis",
          breaks = c(1, 11, 26, max(p_utm$id, na.rm = T)),
          textNA = 'Sem registo',
          border.col = "white",
          style = "fixed",
          title = "Registos por\ndistrito",
          lwd = .01,
          border.alpha = .2) +
  tm_borders(
    col = 'white',
    lwd = .5,
    lty = "solid",
    alpha = .5,
    zindex = NA,
    group = NA
  ) +
  tm_style("gray") +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.6,
    legend.position = c("right","bottom"),
    legend.bg.color = NA,
    legend.bg.alpha = 1)
p1
# Export ----
tmap_save(p1, paste0("./outputs/", 'registos_semana_', ini, '_', end, '.png'),
          width=400, height=500, scale = 0.8,
          dpi = 150, asp = 0)
