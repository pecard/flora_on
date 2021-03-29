# Reduce to region function using polygons
f_map22poly <- function(emap = emap, egeo = egeo){
  lc <-
    tibble(class = emap$reduceRegion(
      reducer= ee$Reducer$toList(),
      maxPixels = 1e9,
      geometry =  egeo
    )$values()$get(0)$getInfo()
    )
}

f_map2point <- function(emap = emap, egeo = egeo){
  data <- emap$sampleRegions(
    collection = egeo,
    scale = 100,
    geometries=TRUE
  )
}

x <- c("165 239 210", "111 45 93")
sapply(strsplit(x, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

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

