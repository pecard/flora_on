## Get Data from Inaturalist aggregated from the Flora-on Project ##

# Load packages ----
kpacks <- c('reticulate', 'ps', "rgee",
            'rinat','tidyverse', 'extrafont',
            'sf', 'data.table', 'lubridate' , 'viridis',
            'ggtext', 'patchwork', 'ggridges', 'ragg',
<<<<<<< HEAD
            'jsonlite', 'httr', 'geojsonio','igraph',  'bipartite',
            'grDevices')
=======
            'jsonlite', 'httr', 'geojsonio')
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# Initiate EarthEngine ----
ee$Authenticate()
ee$Initialize()
#ee_Initialize(email = 'pauloeducardoso@gmail.com') # account email

# Locale to plot names in PT
#Sys.setlocale("LC_ALL", "Portuguese_Portugal.1252")

# source helper funs ----
source('./R/help_funs.R')

# Source base data
source('./R/refData.R')

# Plots ----
source('./R/f_ggtheme.R') # source funs and ggplot themes

# Read INat Flora-on data ----
fon_month <- fread('./data-raw/observations-295874.csv') # jan2023
<<<<<<< HEAD

=======
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
# Adjust column classes ----
fon_month[, datetime := ymd(observed_on) ] # Coerce to datetime
fon_month[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day

setkeyv(fon_month, c('datetime', 'scientific_name'))

summary(fon_month$datetime)

# Start End Dates ----
ini <- min(fon_month$datetime)
end <- max(fon_month$datetime)

# Month summaries: ALL ----
record_day <- fon_month[between(observed_on, ini, end)][, .(count = .N), by = day]

fon_month[observed_on %between% c(ini, end)][, .(count = .N)]

<<<<<<< HEAD
=======

>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
# Remarkable taxa ----
# fon[taxon_id == 424478] # L. ricardoi
# fon[taxon_id == 357867] # Adonis microcarpa
# fon[taxon_id == 158029] # Adonis annua

# Filter and Coerce RG month data to sf ----
fonvalid <-
  fon_month[time_observed_at %between% c(ini, end) &
              quality_grade == 'research'][
<<<<<<< HEAD
                , c('id', 'taxon_id', 'scientific_name', 'latitude',
                    'longitude', 'time_observed_at', 'url')]
=======
    , c('id', 'taxon_id', 'scientific_name', 'latitude',
        'longitude', 'time_observed_at', 'url')]
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e

valid_sf <-
  fonvalid %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Top five RG obs ----
top5 <-
  fonvalid[ , .(count = .N), by = 'scientific_name'][
    order(-count)][1:5, ]

# Spatial aggregate : UTM10x10km ----
p_utm <-
  aggregate(valid_sf['id'], utm10['UTM'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))

# Spatial aggregate : UTM10x10km Madeira ----
p_utm_mad <-
  aggregate(fon_month[, c('id', 'scientific_name', 'latitude', 'longitude',
                          'time_observed_at', 'url')] %>%
              st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
            , utm10mad['id'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))

# Spatial aggregate : UTM10x10km Acores ----
p_utm_aco <-
  aggregate(
    fon_month[, c('id', 'scientific_name', 'latitude', 'longitude',
                  'time_observed_at', 'url')] %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
    , utm10aco['id'],
    FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))

<<<<<<< HEAD
=======

>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
# Plot UTM map ----
p1 <-
  ggplot() +
  geom_sf(data = admin0_cont, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, data = p_utm) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(title = paste("Total de Registos 'RG': ",
                     (sum(p_utm$id, na.rm = T) +
                        sum(p_utm_mad$id, na.rm = T))),
       subtitle = paste(ini, '-', end),
       caption = 'Dados do projeto Flora-on - Biodiversity4all',
       x = NULL, y = NULL)  +
  theme_map() +
  guides(fill = guide_legend(title.position = "top", title.hjust = .5,
                             label.position = "bottom",
                             legend.margin=unit(0, "lines")))
p1c <-
  p1 +
  theme(plot.margin = margin(2,250,2,250),
        panel.spacing = unit(0, "mm"))
p1c

p1mad <-
  ggplot() +
  geom_sf(data = admin_madeira, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, alpha = .9, data = p_utm_mad) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(x = NULL, y = NULL, title = 'Madeira')  +
  theme_map_mad() +
  guides(fill='none')
p1mad

p1aco <-
  ggplot() +
  geom_sf(data = admin_acores_centr, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, alpha = .9, data = p_utm_aco) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(x = NULL, y = NULL, title = 'Açores - grupo central')  +
  theme_map_mad() +
  guides(fill=FALSE)
p1aco

# Plot Obs per day ----
p2 <- record_day %>%
  ggplot(aes(x = as.Date(day), y = count)) +
  geom_line(linewidth = 1.2, colour = '#3b528bff') +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_date(date_labels = "%d %b", breaks = '3 days', expand = c(0.1,0)) +
  labs(x = NULL, y='Observações/dia',
       title = paste("Registos diários em Portugal")) +
  theme_plot()
p2

# Plot Top 5 ----
p3 <-
  ggplot(aes(x=reorder(scientific_name, -count), y = count), data = top5) +
  geom_col(fill = '#21908CFF') +
  coord_flip() +
  labs(x = NULL, y='Registos',
       title = paste("Mais validadas")) +
  geom_text(aes(label = top5$scientific_name, y = 1),
            color = 'grey23',  size = 3, hjust = 0,
            fontface = "italic") +
  theme_plot2()
p3

<<<<<<< HEAD
# Month Top 5 taxa ----
=======


>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
top5taxa <-
  fon_month %>%
  filter(scientific_name %in% top5$scientific_name)

<<<<<<< HEAD
# sf object
top5taxa_sf <-
  top5taxa %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
# Coerce points to ee object
=======
# sf object ----
top5taxa_sf <-
  top5taxa %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
# Coerce points to ee object ----
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
ptos_ee <-
  st_geometry(top5taxa_sf) %>%
  sf_as_ee()

# EarthEngine - Use Corine CLC ----
clc18 = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover')

<<<<<<< HEAD
# Extract CORINE LC to ee points ----
=======
# Extract CORINE landcover to ee points ----
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
top5taxa_clc <- f_map2point(clc18, ptos_ee)

# Coerce eeFeature to DT ----
top5taxa_clc <- rgee::ee_as_sf(top5taxa_clc)
<<<<<<< HEAD
top5taxa_clc$taxonname <- top5taxa$scientific_name # taxon.name
=======
top5taxa_clc$taxonname <- top5taxa$taxon.name
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
st_geometry(top5taxa_clc) <- NULL
top5taxa_clc <- setDT(top5taxa_clc)

# Summarise table
summary_clc <- top5taxa_clc[ , .(count = .N), by = 'landcover'][
<<<<<<< HEAD
  order(-count)] %>%
  inner_join(colourshex, by = c('landcover' = 'CLC_CODE'))

=======
  order(-count)] %>% inner_join(colourshex,
                                by = c('landcover' = 'CLC_CODE'))
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e
summary_clc$landcover <- as.character(summary_clc$landcover)

# CLC legend colours ----
cols = summary_clc$hexcode[1:6]
brks <- as.character(summary_clc$landcover)[1:6]
labs <- summary_clc$LabelCLC[1:6]

p4 <- ggplot(aes(x=reorder(landcover, -count), y = count, fill = landcover),
             data = summary_clc[1:6, ]) +
  geom_bar(stat = 'identity') +
  scale_fill_manual('Legenda CLC',
                    values = cols,
                    breaks = brks,
                    labels = labs) +
  theme_plotCLC() +
  guides(fill=guide_legend(nrow = 5,byrow=TRUE,
                           title.position = "top")) +
  labs(title = 'Uso do solo (CLC)', y = 'Registos')

p4

final <-
  p1c +
  inset_element(p2, .65, .66, 1, 1, align_to = 'full') +
  inset_element(p3, .65, .40, 1, .65, align_to = 'full') +
  inset_element(p4, .65, .00, .95, .39, align_to = 'full') +
  inset_element(p1mad, -0.30, .17, .47, .33, align_to = 'full') +
  inset_element(p1aco, -0.15, .45, .51, .68, align_to = 'full')


ragg::agg_png(here::here("figures",
                         paste0("README-example_pt",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
              res = 320, width = 250, height = 170, units = "mm")
final

dev.off()

<<<<<<< HEAD
# Flora e CLC Network ----

library(bipartite)
library(grDevices)

#clcnet_top <- summary_clc[1:6, ]$landcover
clcnet_top <- summary_clc[1:6, ][ , val := 1]

clcnet <- top5taxa_clc[ , val := 1][
  landcover %in% clcnet_top$landcover]

clcnetcol <-
  merge(x = clcnet, y = colourshex[ , c(4,5,6)], by.x="landcover",
        by.y="CLC_CODE")

clcnetwd <-
  clcnetcol %>%
  pivot_wider(id_cols = "taxonname",
              names_from = "Labelplot",
              values_from = "val",
              values_fn = list(val = length))
clcnetm <-
  data.matrix(clcnetwd[ ,2:7])
row.names(clcnetm) <- clcnetwd$taxonname
clcnetm[is.na(clcnetm)] <- 0

bipcols <- filter(colourshex, Labelplot %in% colnames(clcnetm))$hexcode

bipartite::plotweb(clcnetm, abuns.type='additional',
                   arrow="up.center",
                   text.rot=0,
                   col.interaction = adjustcolor('#21908CFF',
                                                            alpha.f = 0.2),
                   bor.col.interaction = NA,
                   col.high=bipcols,
                   col.low = '#21908CFF'
)
ragg::agg_png(here::here("figures",
                         paste0("README-example_network_pt",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
              res = 320, width = 130, height = 80, units = "mm")

bipartite::plotweb(clcnetm, abuns.type='additional',
                   arrow="up.center",
                   text.rot=0,
                   col.interaction = adjustcolor('#21908CFF',
                                                            alpha.f = 0.2),
                   bor.col.interaction = NA,
                   col.high=bipcols,
                   col.low = '#21908CFF'
)

dev.off()

=======
>>>>>>> e063050124ee08850fddb606fc2f2447116b5f8e

