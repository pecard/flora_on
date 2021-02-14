# Admin data level 1 ----
pt_adm0 <- getData('GADM', country='Portugal', level=0)
saveRDS(pt_adm0, './data/admin0_pt.rds')

adm1 <- readRDS('./data/admin1_pt.rds')

# Bounding box for continental PT ----
ptc_lim = data.frame(ylim=c(35, 43), xlim=c(-12, -6.0))
mad_lim = data.frame(ylim=c(32, 33.2), xlim=c(-17.6, -15.9))

pt_bbox <-
  st_bbox(c(xmin=ptc_lim$xlim[1],
            xmax=ptc_lim$xlim[2],
            ymin=ptc_lim$ylim[1],
            ymax=ptc_lim$ylim[2]))

ma_bbox <-
  st_bbox(c(xmin=mad_lim$xlim[1],
            xmax=mad_lim$xlim[2],
            ymin=mad_lim$ylim[1],
            ymax=mad_lim$ylim[2]))

admin1_cont <-
  sf::st_as_sf(adm1) %>%
  st_crop(pt_bbox)
admin0_cont <-
  sf::st_as_sf(pt_adm0) %>%
  st_crop(pt_bbox)
admin0_mad <-
  sf::st_as_sf(pt_adm0) %>%
  st_crop(ma_bbox)
plot(admin0_mad['NAME_0'])
saveRDS(adm1_cont, './data/admin1_cont.rds')
saveRDS(adm0_cont, './data/admin0_cont.rds')

# UTM 10x10
utm10 <- sf::read_sf("D:/Google Drive/sig/grids/utm/UTM10x10_3763.shp") %>%
  sf::st_transform(4326)
saveRDS(utm10, "./data/utm10.rds")


