
t_lric <- iNat(project_id = 'flora-on', year = 2020, taxon_id = 424478) # L ricardoi
iNat(project_id = 'flora-on', year = 2020, taxon_id = 56024)

flor_lst <- vector('list')
top5taxa_id <- unique(top5taxa$taxon_id)
for(i in seq_along(top5taxa_id)){
  flor_lst[[i]] <- iNat(project_id = 'flora-on', year = 2020,
                        taxon_id = top5taxa_id[i])
}
fl_list <- bind_rows(flor_lst)

# dt_orq <- setDT(testpt)
# dt_orq[, datetime := ymd(observed_on) ] # Coerce to datetime
# dt_orq[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day
# setkeyv(dt_orq, c('datetime', 'taxon.name'))
# summary(dt_orq$datetime)
# top5orq <- dt_orq[
#   , .(count = .N), by = 'taxon.name'][
#     order(-count)][1:5, ]
# top5orq
#
#testpt$taxon.name


floracao <- lapply(fl_list$annotations, `[[`, "concatenated_attr_val")
floracao <- as.data.frame(data.table::transpose(floracao))
floracao$data <- fl_list$observed_on
floracao$taxon <- fl_list$taxon.name
floracao <- floracao %>%
  pivot_longer(!c(taxon, data), names_to = 'tipo', values_to = 'flower') %>%
  filter(flower == '12|13') %>%
  select(data, taxon, flower) %>%
  mutate(data = as.Date(data),
         dy = yday(data))
p5 <- floracao %>%
  ggplot(aes(x = data, fill = taxon)) +
  stat_density(geom = "area", adjust = 1,
               kernel = "gaussian", aes(y = ..scaled..),
               alpha = .4) +
  facet_grid(taxon~., scales = 'free_y') +
  theme_plot2() +
  theme(strip.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b",
               expand = c(0,0))

floracao %>%
  ggplot(aes(y = taxon, x = data, fill = taxon)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, colour = NA) +
  #theme_ridges() +
  #facet_grid(taxon~., scales = 'free_y') +
  theme_plot2() +
  theme(strip.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b")

ragg::agg_png(here::here("figures",
                         paste0("README-example_pt",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
              res = 320, width = 120, height = 50, units = "mm")
p5

dev.off()
