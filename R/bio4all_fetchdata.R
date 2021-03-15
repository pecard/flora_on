## Get Data from Inaturalist aggregated from the Flora-on Project ##


# Locale to plot names in PT
Sys.setlocale()
# Sys.getlocale("LC_TIME") "English_United Kingdom.1252"
Sys.setlocale("LC_ALL", "Portuguese_Portugal.1252")

# Load packages ----
#remotes::install_github("ropensci/rinat") # install from github repo
kpacks <- c('rinat', 'tidyverse', 'extrafont', 'sf', 'data.table', 'lubridate', 'viridis', 'raster',
            'ggtext', 'patchwork')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# Load data Admin data ----
admin0_cont <- readRDS('./data/admin0_cont.rds')
admin1_cont <- readRDS('./data/admin1_cont.rds')

# Load utm grid ----
utm10 <- readRDS('./data/utm10.rds')

# Source functions ----
source('./R/f_ggtheme.R') # ggplot theme

# Get data from Inat Flora-on ----
fon <- get_inat_obs_project('flora-on')

# Data.table ----
setDT(fon) # coerce to DT

# Adjust classes
fon[, datetime := ymd(observed_on) ] # Coerce to datetime
fon[, day := as.POSIXct(trunc(datetime, 'days')) ] # round day
setkeyv(fon, c('datetime', 'taxon.name'))
summary(fon$datetime)

# Records from the last 8 days ----
ini <- Sys.Date() - 7
end <- Sys.Date()

# Week summaries ----
record_day <- fon[between(observed_on, ini, end)][, .(count = .N), by = day]

fon[observed_on %between% c(ini, end)][, .(count = .N)]

# Top five obs ----
top5 <- fon[observed_on %between% c(ini, end)][, .(count = .N), by = 'taxon.name'][order(-count)][1:5,]

fon[taxon_id == 424478] # L. ricardoi

# Coerce DT to sf ----
fonweek <-
  fon[time_observed_at %between% c(ini, end) & quality_grade == 'research'][
    , c('id', 'latitude', 'longitude', 'time_observed_at')] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Aggregate observations by Level1 (District) ----
# p_ag1 <-
#   aggregate(fonweek, admin1_cont['NAME_1'],
#             FUN = function(x) length(x)) %>%
#   mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
#                    labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
#   )

# Aggregate by UTM10x10km ----
p_utm <-
  aggregate(fonweek, utm10['UTM'],
            FUN = function(x) length(x)) %>%
  mutate(ncl = cut(id, breaks = c(0, 10, 25, 50, 75, +Inf),
                   labels = c('1-10', '11-25', '26-50', '51-75', '>75'))
  ) %>% filter(!is.na(id))


# Plot UTM map ----
p1 <- ggplot() +
  geom_sf(data = admin0_cont, fill = 'grey70', color = NA, size = .01) +
  geom_sf(aes(fill = ncl), color = NA, data = p_utm) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_d(name='Registos', option = "viridis",
                       direction = -1, drop=FALSE) +
  labs(title = paste("Total de Regitos: ", sum(p_utm$id, na.rm = T)),
       subtitle = paste(ini, '-', end),
       caption = 'Dados do projeto Flora-on - Biodiversity4all',
       x = NULL, y = NULL)  +
  theme_map() +
  guides(fill = guide_legend(title.position = "top", title.hjust = .5,
                             label.position = "bottom",
                             legend.margin=unit(0, "lines")))
p1
# ggsave('figures/README-example_pt.png',
#        plot = p1,
#        height = 120,
#        width = 80,
#        units = 'mm',
#        dpi = 300
# )

# Plot Obs per day ----
p2 <- record_day %>%
  ggplot(aes(x = as.Date(day), y = count)) +
  geom_line(size = 1.2, colour = '#3b528bff') +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_labels = "%a, %d %b", breaks = '1 day') +
  labs(x = NULL, y='Observações/dia',
       title = paste("Registos diários")) +
  theme_plot()
p2
# ggsave('figures/recordsperday_pt.png',
#        plot = p2,
#        height = 80,
#        width = 120,
#        units = 'mm',
#        dpi = 300
# )


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
  # geom_label(aes(label = top5$taxon.name, y = 1),
  #           fill = 'white', colour = 'grey50', size = 2, hjust = 0,
  #           fontface = "italic", alpha = 0.5) +
  theme_plot2()

p3

# Test patchwork layout

# layout <- "
# AAAABB
# AAAA##
# AAAACC
# "
# p1 + p2 + p3 +
#   plot_layout(design = layout,
#               widths = unit(c(1, 1), c('cm', 'cm')),
#               heights = unit(c(3, 1), c('cm', 'cm'))) +
#   plot_annotation(theme = theme(plot.background =
#                                   element_rect(fill = "#f5f5f2",
#                                                color = NA)))

# Save plot ----
ggsave('figures/README-example_pt.png',
       plot = p1 + p2 / p3,
       height = 150,
       width = 120,
       units = 'mm',
       dpi = 300
)

# Test text plot
# https://github.com/jennschilling/covid/blob/main/vaccine_dist_2021_03.Rmd
# Plot Formatting
#font <- "Gill Sans MT"
#theme_set(theme_map(base_family = font))

