# Base data ----
pt_adm0 <- readRDS('./data/admin0_pt.rds')
adm1 <- readRDS('./data/admin1_pt.rds')
admin0_cont <- readRDS('./data/admin0_cont.rds')
admin1_cont <- readRDS('./data/admin1_cont.rds')
admin_madeira <- readRDS('./data/madeira.rds')
utm10 <- readRDS('./data/utm10.rds')
utm10mad <- readRDS('./data/utm10mad.rds')
utm10mad <- readRDS('./data/utm10mad.rds')

# Bounding box for continental PT ----
ptc_lim = data.frame(ylim=c(35, 43), xlim=c(-12, -6.0))
mad_lim = data.frame(ylim=c(32, 33.2), xlim=c(-17.6, -15.9))

pt_bbox <-
  sf::st_bbox(c(xmin=ptc_lim$xlim[1],
            xmax=ptc_lim$xlim[2],
            ymin=ptc_lim$ylim[1],
            ymax=ptc_lim$ylim[2]))

ma_bbox <-
  sf::st_bbox(c(xmin=mad_lim$xlim[1],
            xmax=mad_lim$xlim[2],
            ymin=mad_lim$ylim[1],
            ymax=mad_lim$ylim[2]))

# CLC colour scheme
# read corine color palette
colours <- readxl::read_xls('./data-raw/clc2000legend.xls')
colourshex <- as_tibble(str_split_fixed(colours$RGB, "-", 3)) %>%
  mutate_if(is.character, as.integer)
colourshex$CLC_CODE <- colours$CLC_CODE
colourshex$Labelplot <- colours$Labelplot
colourshex <- colourshex %>%
  filter(complete.cases(.)) %>%
  mutate(hexcode = rgb(V1, V2, V3, maxColorValue=255),
         LabelCLC = paste(CLC_CODE, Labelplot))

# admin1_cont <-
#   sf::st_as_sf(adm1) %>%
#   sf::st_crop(pt_bbox)

# admin0_cont <-
#   sf::st_as_sf(pt_adm0) %>%
#   st_crop(pt_bbox)
#
# admin0_mad <-
#   sf::st_as_sf(pt_adm0) %>%
#   st_crop(ma_bbox)

#plot(admin0_mad['NAME_0'])
#saveRDS(adm1_cont, './data/admin1_cont.rds')
#saveRDS(adm0_cont, './data/admin0_cont.rds')

# UTM 10x10
# utm10 <- readRDS('./data/utm10.rds') %>%
#   sf::st_transform(4326)
#
# utm10 <- readRDS('./data/utm10.rds') %>%
#   sf::st_transform(4326)

#saveRDS(utm10, "./data/utm10.rds")


