# packages
pacman::p_load('reticulate', 'ps', "rgee", 
               "tidyverse", "sf", "ggplot2", "patchwork",
               'googledrive', 'stars', 'plotKML', 'readxl',
               'networkD3', 'OpenLand')

ee_Initialize(email = 'pauloeducardoso@gmail.com')

# CORINE
clc18 = ee$Image('COPERNICUS/CORINE/V20/100m/2018')$select('landcover');
clc00 = ee$Image('COPERNICUS/CORINE/V20/100m/2000')$select('landcover');
clc1800 = clc18$multiply(1000)$add(clc00);

# CLC colour scheme
# read corine color palette
colours <- readxl::read_xls('D:/Dropbox/programacao/gee/clc2000legend.xls')
colourshex <- as_tibble(str_split_fixed(colours$RGB, "-", 3)) %>%
  mutate_if(is.character, as.integer)
colourshex$CLC_CODE <- colours$CLC_CODE
colourshex$Labelplot <- colours$Labelplot
colourshex <- colourshex %>%
  filter(complete.cases(.)) %>%
  mutate(hexcode = rgb(V1, V2, V3, maxColorValue=255),
         LabelCLC = paste(CLC_CODE, Labelplot))

# ROI
roi <- st_buffer(sf::st_sfc(st_point(c(-8.2575414,37.9295831))), 0.1) %>% sf::st_set_crs(4326)
roi <- sf::st_sfc(st_point(c(-8.2575414,37.9295831))) %>% 
  sf::st_set_crs(4326) %>% st_transform(3857)
roi <- roi %>% st_buffer(20000)
roi_ee <- roi %>% sf_as_ee()

# Map View
Map$setCenter(-8.2575414,37.9295831, 10)
Map$addLayer(clc18, {}, 'Land Cover 18', opacity = 0.4) +
  Map$addLayer(roi_ee, {}, 'roi',  opacity = 0.4)

# Reduce to region
vListchange <- tibble(change = clc1800$reduceRegion(
  reducer= ee$Reducer$toList(),
  maxPixels = 1e9,
  geometry =  roi_ee
)$values()$get(0)$getInfo()
)

# Summarise
change1800 <- vListchange %>% separate(change, into = c('clc18', 'clc00'), sep = 3, remove = F)
change1800un <- distinct(change1800)
legend_tab <- tibble(categoryValue = as.numeric(unique(c(change1800un$clc18,change1800un$clc18)))) %>%
  left_join(select(colourshex, CLC_CODE, color= hexcode), by = c('categoryValue' = 'CLC_CODE')) %>%
  mutate(categoryName=as.factor(categoryValue)) %>%
  select(categoryValue, categoryName, color)

summchange1800 <- change1800 %>% group_by(change) %>%
  summarise(area_ha = n(), .groups = 'drop') %>%
  inner_join(distinct(change1800), by = c('change' = 'change')) %>%
  mutate(changed = clc18 != clc00,
         CLC_CODE = as.numeric(clc18),
         fromto = paste(clc00, clc18, sep = '-')) %>%
  inner_join(select(colourshex, CLC_CODE, hexcode), by = c('CLC_CODE' = 'CLC_CODE'))

gralt <- summchange1800 %>% filter(area_ha > 1000)
gr_clccode <- unique(c(as.numeric(gralt$clc18), as.numeric(gralt$clc00)))
gr_legend_tab <- 
  legend_tab %>% 
  filter(categoryValue %in% gr_clccode)
gr_legend_tab$categoryName <- as.factor(as.character(gr_legend_tab$categoryName))

# ChordDiagram inputs
conting_tab <- tibble(Period = '2000-2018',
                      From = as.integer(summchange1800$clc00),
                      To = as.integer(summchange1800$clc18),
                      km2 = summchange1800$area_ha/1000,
                      QtPixel = summchange1800$area_ha,
                      Interval = 6,
                      yearFrom = 2000,
                      yearTo = 2018)

gr_conting_tab <- tibble(Period = '2000-2018',
                         From = as.integer(gralt$clc00),
                         To = as.integer(gralt$clc18),
                         km2 = gralt$area_ha/1000,
                         QtPixel = gralt$area_ha,
                         Interval = 6,
                         yearFrom = 2000,
                         yearTo = 2018)

png(filename = 'transition_matrix_diagfram.png', height = 17, width = 18,
    units = "cm", res = 300)
chordDiagramLand(dataset = conting_tab, legendtable = legend_tab, legtitle = "Legenda CLC",
                 legendsize = .7)

dev.off()

png(filename = 'transition_matrix_diagfram.png', height = 14, width = 18,
    units = "cm", res = 300)
chordDiagramLand(dataset = gr_conting_tab, legendtable = gr_legend_tab, legtitle = "Legenda CLC",
                 legendsize = 1)

dev.off()
