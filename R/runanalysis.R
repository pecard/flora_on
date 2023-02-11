## Get Data from Inaturalist aggregated from the Flora-on Project ##

# Load packages ----
kpacks <- c('reticulate', 'ps', "rgee",
            'rinat','tidyverse', 'extrafont',
            'sf', 'data.table', 'lubridate' , 'viridis',
            'ggtext', 'patchwork', 'ggridges', 'ragg',
            'jsonlite', 'httr', 'geojsonio')
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
source('./R/getRefData.R')

# Plots ----
source('./R/f_ggtheme.R') # source funs and ggplot themes

# Read INat Flora-on data ----
fon <- readRDS(here::here('data/bio4alldata.rds'))
setDT(fon) # coerce to DT

#fonbbtz <- iNat(project_id = 'i-bioblitz-flora-portugal')
#fonbbtz <- iNat(project_id = 'i-bioblitz-flora-portugal')
fon_month <- fread('./data-raw/observations-295874.csv') # jan2023
# Adjust column classes ----
fon_month[, datetime := ymd(observed_on) ] # Coerce to datetime
fon_month[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day

setkeyv(fon_month, c('datetime', 'scientific_name'))

#saveRDS(fon, here::here('data/bio4alldata.rds'))

# Adjust column classes ----
#fon[, datetime := ymd(observed_on) ] # Coerce to datetime
#fon[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day
#setkeyv(fon, c('datetime', 'taxon.name'))

summary(fon_month$datetime)

# Start End Dates ----
ini <- min(fon_month$datetime)
end <- max(fon_month$datetime)

# Month summaries: ALL ----
record_day <- fon_month[between(observed_on, ini, end)][, .(count = .N), by = day]

fon_month[observed_on %between% c(ini, end)][, .(count = .N)]


# Remarkable taxa ----
# fon[taxon_id == 424478] # L. ricardoi
# fon[taxon_id == 357867] # Adonis microcarpa
# fon[taxon_id == 158029] # Adonis annua


# Filter and Coerce RG month data to sf ----
fonvalid <-
  fon_month[time_observed_at %between% c(ini, end) &
              quality_grade == 'research'][
    , c('id', 'taxon_id', 'scientific_name', 'latitude',
        'longitude', 'time_observed_at', 'url')]

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



# Plot UTM map ----
p1 <-
  ggplot() +
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
  guides(fill='none')
p1mad

# Plot Obs per day ----
p2 <- record_day %>%
  ggplot(aes(x = as.Date(day), y = count)) +
  geom_line(linewidth = 1.2, colour = '#3b528bff') +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_date(date_labels = "%d %b", breaks = '3 days', expand = c(0.1,0)) +
  labs(x = NULL, y='Observações/dia',
       title = paste("Num. registos diários em Portugal")) +
  theme_plot()
p2

# Plot Top 5 ----
p3 <-
  ggplot(aes(x=reorder(scientific_name, -count), y = count), data = top5) +
  geom_col(fill = '#21908CFF') +
  coord_flip() +
  labs(x = NULL, y='Registos',
       title = paste("Mais observadas")) +
  geom_text(aes(label = top5$scientific_name, y = 1),
            color = 'grey23',  size = 3, hjust = 0,
            fontface = "italic") +
  theme_plot2()
p3


# EarthEngine - Use Corine CLC ----
clc18 = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover')

# Coerce points to ee object ----
top5taxa <- fon_month %>% filter(scientific_name %in% top5$scientific_name) %>%
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


