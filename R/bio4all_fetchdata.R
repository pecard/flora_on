## Get Data from Inaturalist aggregated with Flora-on Project ##

# Load packages ----
#remotes::install_github("ropensci/rinat")
pacman::p_load(rinat, tidyverse, data.table, raster, lubridate, sf, tmap)

# Get data from Flora-on ----
fon <- get_inat_obs_project('flora-on')

# Data.table ----
setDT(fon)
fon[, created_at := ymd_hms(created_at) ] # posixct
fon[, day := as.POSIXct(trunc(created_at, 'days')) ] # round day
setkey(fon, created_at) # DT key

# Summaries ----
fon[created_at > (Sys.Date() - 6)][, .(count = .N), by = day]
fon[created_at > (Sys.Date() - 6)][, .(count = .N)]

# records from the last 7 days ----
fonweek <-
  fon[created_at > (Sys.Date() - 6)][, 1:11]

# Coerce to sf spatial ----
fonweekg <-
  fonweek %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Admin data level 1 ----
pt_adm1 <- getData('GADM', country='Portugal', level=1)

# Bounding box for continental PT ----
ptc_lim = data.frame(ylim=c(35, 43), xlim=c(-12, -6.0))
pt_bbox <-
  st_bbox(c(xmin=ptc_lim$xlim[1],
            xmax=ptc_lim$xlim[2],
            ymin=ptc_lim$ylim[1],
            ymax=ptc_lim$ylim[2]))
ptc_adm1 <-
  sf::st_as_sf(pt_adm1) %>%
  st_crop(pt_bbox)

# Aggregate observations by Level1 (District)
p_ag1 <-
  aggregate(fonweekg, ptc_adm1['NAME_1'],
            FUN = function(x) length(x))

# Plot with tmap ----
tmap_mode("plot")

p1 <- tm_shape(p_ag1) +
  tm_fill(col = 'id',
          palette =  "viridis",
          breaks = c(1, 11, 51, 150),
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

# Export ----
tmap_save(p1, "D:/paulo/spbotanica/registos_semana1.png",
          width=400, height=500, scale = 0.8,
          dpi = 150, asp = 0)
