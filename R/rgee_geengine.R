# Initiate EarthEngine ----
ee_Initialize(email = 'pauloeducardoso@gmail.com') # account email

# # if warning, upgrade ----
rgee::ee_install_upgrade(
  version = NULL,
  earthengine_env = Sys.getenv("EARTHENGINE_ENV")
)
rgee::ee_check()

#remotes::install_github("ropensci/rinat") # install from github repo
kpacks <- c('rinat','tidyverse', 'extrafont',
            'sf', 'data.table', 'lubridate' , 'viridis',
            'ggtext', 'patchwork', 'ggridges', 'ragg',
            'jsonlite', 'httr')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)
