## Get Data from Inaturalist aggregated from the Flora-on Project ##

# Load packages ----
#remotes::install_github("ropensci/rinat") # install from github repo
kpacks <- c('reticulate', 'ps', "rgee",
            'rinat','tidyverse', 'extrafont',
            'sf', 'data.table', 'lubridate' , 'viridis',
            'ggtext', 'patchwork', 'ggridges')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# Initiate EarthEngine ----
ee_Initialize(email = 'pauloeducardoso@gmail.com') # account email

#remotes::install_github("ropensci/rinat") # install from github repo
kpacks <- c('rinat','tidyverse', 'extrafont',
            'sf', 'data.table', 'lubridate' , 'viridis',
            'ggtext', 'patchwork', 'ggridges', 'ragg',
            'jsonlite', 'httr')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# Locale to plot names in PT
#Sys.setlocale()
# Sys.getlocale("LC_TIME") "English_United Kingdom.1252"
Sys.setlocale("LC_ALL", "Portuguese_Portugal.1252")

# source funs ----
source('./R/help_funs.R')

# Load data Admin data ----
admin0_cont <- readRDS('./data/admin0_cont.rds')
admin1_cont <- readRDS('./data/admin1_cont.rds')
admin_madeira <- readRDS('./data/madeira.rds')

# Load utm grids ----
utm10 <- readRDS('./data/utm10.rds')
utm10mad <- readRDS('./data/utm10mad.rds')

# Get data from Inat Flora-on ----
fon <- readRDS(here::here('data/bio4alldata.rds'))
#fon <- get_inat_obs_project('flora-on', raw = TRUE)
setDT(fon) # coerce to DT
#saveRDS(fon, here::here('data/bio4alldata.rds'))

# Adjust column classes ----
fon[, datetime := ymd(observed_on) ] # Coerce to datetime
fon[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day
setkeyv(fon, c('datetime', 'taxon.name'))
summary(fon$datetime)

# Records from the last 8 days ----
ini <- Sys.Date() - 7
end <- Sys.Date()

# Other, Specific Dates ----
#ini <- '2021-03-01'
#end <- '2021-03-19'

# Week summaries ----
record_day <- fon[between(observed_on, ini, end)][, .(count = .N), by = day]

fon[observed_on %between% c(ini, end)][, .(count = .N)]

# Top five obs ----
top5 <- fon[observed_on %between% c(ini, end)][
  , .(count = .N), by = 'taxon.name'][
    order(-count)][1:5, ]

# Remarkable taxa ----
# fon[taxon_id == 424478] # L. ricardoi
# fon[taxon_id == 357867] # Adonis microcarpa
# fon[taxon_id == 158029] # Adonis annua


# Filter and Coerce week data to sf ----
fonweek <-
  fon[time_observed_at %between% c(ini, end) & quality_grade == 'research'][
    , c('id', 'taxon_id', 'taxon.name', 'latitude',
        'longitude', 'time_observed_at', 'uri')]

week_sf <- fonweek %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Spatial aggregate : UTM10x10km ----
p_utm <-
  aggregate(week_sf['id'], utm10['UTM'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))

# Spatial aggregate : UTM10x10km Madeira ----
p_utm_mad <-
  aggregate(fon[, c('id', 'taxon.name', 'latitude', 'longitude',
                    'time_observed_at', 'uri')] %>%
              st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
            , utm10mad['id'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))


# Plots ----
source('./R/f_ggtheme.R') # source funs and ggplot themes

# Plot UTM map ----
p1 <- ggplot() +
  geom_sf(data = admin0_cont, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, data = p_utm) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(title = paste("Total de Registos: ",
                     (sum(p_utm$id, na.rm = T) +
                        sum(p_utm_mad$id, na.rm = T))),
       subtitle = paste(ini, '-', end),
       caption = 'Dados do projeto Flora-on - Biodiversity4all',
       x = NULL, y = NULL)  +
  theme_map() +
  guides(fill = guide_legend(title.position = "top", title.hjust = .5,
                             label.position = "bottom",
                             legend.margin=unit(0, "lines")))
p1 <-
  p1 +
  theme(plot.margin = margin(1,200,1,100),
        panel.spacing = unit(0, "mm"))

p1mad <- ggplot() +
  geom_sf(data = admin_madeira, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, alpha = .9, data = p_utm_mad) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(x = NULL, y = NULL, title = 'Madeira')  +
  theme_map_mad() +
  guides(fill=FALSE)
p1mad

# Plot Obs per day ----
p2 <- record_day %>%
  ggplot(aes(x = as.Date(day), y = count)) +
  geom_line(size = 1.2, colour = '#3b528bff') +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_date(date_labels = "%d %b", breaks = '1 day') +
  labs(x = NULL, y='Observações/dia',
       title = paste("Registos diários")) +
  theme_plot()
p2

# Plot Top 5 ----
p3 <-
  ggplot(aes(x=reorder(taxon.name, -count), y = count), data = top5) +
  geom_col(fill = '#21908CFF') +
  coord_flip() +
  labs(x = NULL, y='Registos',
       title = paste("Mais observadas")) +
  geom_text(aes(label = top5$taxon.name, y = 1),
            color = 'grey23',  size = 3, hjust = 0,
            fontface = "italic") +
  theme_plot2()
p3


# EarthEngine - Use Corine CLC ----
clc18 = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover')

# Coerce points to ee object ----
top5taxa <- fonweek %>% filter(taxon.name %in% top5$taxon.name) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
ptos_ee <- st_geometry(top5taxa) %>% sf_as_ee()

# Extract CORINE landcover to ee points ----
top5taxa_clc <- f_map2point(clc18, ptos_ee)

# Coerce eeFeature to DT ----
top5taxa_clc <- rgee::ee_as_sf(top5taxa_clc)
top5taxa_clc$taxonname <- top5taxa$taxon.name
st_geometry(top5taxa_clc) <- NULL
top5taxa_clc <- setDT(top5taxa_clc)

# Summarise table
summary_clc <- top5taxa_clc[ , .(count = .N), by = 'landcover'][
  order(-count)] %>% inner_join(colourshex,
                                by = c('landcover' = 'CLC_CODE'))
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
final <- p1 +
  inset_element(p2, .63, .66, 1, 1, align_to = 'full') +
  inset_element(p3, .63, .40, 1, .65, align_to = 'full') +
  inset_element(p4, .63, .00, .95, .39, align_to = 'full') +
  inset_element(p1mad, -0.30, .17, .47, .33, align_to = 'full')


ragg::agg_png(here::here("figures",
                         paste0("README-example_pt",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
              res = 320, width = 200, height = 170, units = "mm")
final

dev.off()


