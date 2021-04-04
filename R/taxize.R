library(tidyverse)
library(taxize)

# inat_flora <- readr::read_csv('D:\\Dropbox\\programacao\\packagedevel\\tabulEIAdata\\Inat_flora20000101_20210101.csv')
# names(inat_flora)

dbsources <- gnr_datasources()

mysp <- unique(fonweek$taxon.name)
nsp = length(mysp)

lref <- list()
myseq <- seq_len(nsp)
for(i in myseq){
  #s=i
  #e=i+39
  #ind = which(myseq == i)
  retr_i<- try(
    gnr_resolve(mysp[i],
                preferred_data_sources = c(180),
                fields = 'all'), silent = T )
  if(is.data.frame(retr_i) ){
    if(nrow(retr_i) > 0){
    lref[[i]] <- retr_i %>%
      select(taxon_id, matched_name,
             classification_path,
             classification_path_ranks)
  }} else lref[[i]] <- data.frame('taxon_id' = '-',
                                 'matched_name' = '-',
                                 'classification_path' = '-',
                                 stringsAsFactors = F)
  Sys.sleep(.5)
  print(paste("resolving: ", i, ". to finish:", length(myseq)-i))
}
tlref <- setDT(bind_rows(lref))[ , taxon_id := as.numeric(taxon_id)]

fonweek <- fonweek[tlref, on = 'taxon_id']
orq <- fonweek %>%
  tidyr::separate_rows(classification_path) %>%
  filter(classification_path == 'Orchidaceae') %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Spatial aggregate : UTM10x10km ----
p_utm_orq <-
  aggregate(orq['id'], utm10['UTM'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 5, 10, 20, +Inf),
                   labels = c('1-5', '6-10', '11-20', '>20'))
  ) %>% filter(!is.na(id))
p1_orq <- ggplot() +
  geom_sf(data = admin0_cont, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, data = p_utm_orq) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(title = paste("Total de Registos: ", sum(p_utm_orq$id, na.rm = T)),
       subtitle = paste(ini, '-', end),
       caption = 'Dados do projeto Flora-on - Biodiversity4all',
       x = NULL, y = NULL)  +
  theme_map() +
  guides(fill = guide_legend(title.position = "top", title.hjust = .5,
                             label.position = "bottom",
                             legend.margin=unit(0, "lines")))
p1_orq
