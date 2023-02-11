library(visNetwork)
library(igraph)
library(qgraph)
#devtools::install_github('hfgolino/EGA')
library(EGAnet)
class(data)
# https://psych-networks.com/r-tutorial-identify-communities-items-networks/

# Reviewers to Research Grade
users <- flory_df$reviewed_by
users <-
  data.frame(reviewer = sapply(users, toString), stringsAsFactors=FALSE)
users$taxon <- floracao$taxon
users$data <- ymd(floracao$data)
users$user_id <- flory_df$user.id
users$name <- flory_df$user.name
users <- filter(users, data > ymd('2021-01-01') )
user_link <- separate_rows(users, reviewer) %>%
  mutate(reviewer = as.numeric(reviewer)) %>%
  filter(user_id != reviewer) %>%
  group_by(user_id, reviewer) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(n > 5) %>%
  pivot_wider(names_from = reviewer, values_from = n, values_fill = 0)

corusers <- cor_auto(user_link[ ,-1])
posi_cor <- Matrix::nearPD(corusers, cor = T)$mat
ega_users <- EGA(user_link[ ,-1], plot.EGA = TRUE, plot.type = 'GGally')

ragg::agg_png(here::here("figures",
                         paste0("README-example_users_network_pt",
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
              res = 320, width = 90, height = 90, units = "mm")

ega_users <- EGA(user_link[ ,-1], plot.EGA = TRUE, plot.type = 'GGally',
                 plot.args = list(vsize = 6, label.size = 0))
dev.off()

graph1 <- qgraph(posi_cor, graph="glasso", layout="spring",
                 sampleSize = nrow(posi_cor),
                 vsize=3, cut=0, maximum=.45, border.width = 1.5,
                 labels = F,
                 title = 'Redes entre utilizadores (mais ativos em 2021')
graph1
nNode <- 20
set.seed(1)
graph2<-qgraph(posi_cor, graph="glasso", layout="spring", sampleSize = nrow(posi_cor),
               vsize=7, cut=0, maximum=.45, border.width=1.5, layout.par =
                 list(init = matrix(rnorm(nNode*2),nNode,2)))
set.seed(2)
graph3<-qgraph(posi_cor, graph="glasso", layout="spring", sampleSize = nrow(posi_cor),
               vsize=7, cut=0, maximum=.45, border.width=1.5,
               layout.par = list(init = matrix(rnorm(nNode*2),nNode,2)))
