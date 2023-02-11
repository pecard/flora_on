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


# Inaturalist API data access
# Use iNaturalist API to request data
iNat <- function(per_page = 200, order = "desc", order_by = "created_at", acc = NULL,
                 captive = NULL, endemic = NULL, geo = NULL, identified = NULL,
                 introduced = NULL, mappable = NULL, native = NULL,  only_id = NULL,
                 out_of_range = NULL, pcid = NULL, photos = NULL, popular = NULL,
                 taxon_is_active = NULL, threatened = NULL, verifiable = "true",
                 id = NULL, not_id = NULL, place_id = NULL, project_id = NULL,
                 rank = NULL, site_id = NULL, taxon_id = NULL, without_taxon_id = NULL,
                 taxon_name = NULL, user_id = NULL, user_login = NULL, day = NULL,
                 month = NULL, year = NULL, term_id = NULL, term_value_id = NULL,
                 without_term_value_id = NULL, acc_above = NULL, acc_below = NULL,
                 d1 = NULL, d2 = NULL, created_d1 = NULL, created_d2 = NULL,
                 created_on = NULL, observed_on = NULL, unobserved_by_user_id = NULL,
                 apply_project_rules_for = NULL, cs = NULL, csa = NULL, csi = NULL,
                 geoprivacy = NULL, taxon_geoprivacy = NULL, hrank = NULL, lrank = NULL,
                 id_above = NULL, id_below = NULL, identifications = NULL, lat = NULL,
                 lng = NULL, radius = NULL, nelat = NULL, nelng = NULL, swlat = NULL,
                 swlng = NULL, list_id = NULL, not_in_project = NULL,
                 not_matching_project_rules_for = NULL, q = NULL, search_on = NULL,
                 quality_grade = NULL, updated_since = NULL, viewer_id = NULL,
                 reviewed = NULL, locale = NULL, preferred_place_id = NULL, ttl = NULL) {

  options(stringsAsFactors = FALSE)
  api <- "https://api.inaturalist.org/v1/observations"

  fetch <- list(per_page, order, order_by, acc, captive, endemic, geo, identified, introduced,
                mappable, native, only_id, out_of_range, pcid, photos, popular, taxon_is_active,
                threatened, verifiable, id, not_id, place_id, project_id, rank, site_id,
                taxon_id, without_taxon_id, taxon_name, user_id, user_login, day, month, year,
                term_id, term_value_id, without_term_value_id, acc_above, acc_below, d1, d2, created_d1,
                created_d2, created_on, observed_on, unobserved_by_user_id, apply_project_rules_for,
                cs, csa, csi, geoprivacy, taxon_geoprivacy, hrank, lrank, id_above, id_below, identifications,
                lat, lng, radius, nelat, nelng, swlat, swlng, list_id, not_in_project, not_matching_project_rules_for,
                q, search_on, quality_grade, updated_since, viewer_id, reviewed, locale, preferred_place_id, ttl)

  names(fetch) <- c("per_page", "order", "order_by", "acc", "captive", "endemic", "geo", "identified", "introduced",
                    "mappable", "native", "only_id", "out_of_range", "pcid", "photos", "popular", "taxon_is_active",
                    "threatened", "verifiable", "id", "not_id", "place_id", "project_id", "rank", "site_id",
                    "taxon_id", "without_taxon_id", "taxon_name", "user_id", "user_login", "day", "month", "year",
                    "term_id", "term_value_id", "without_term_value_id", "acc_above", "acc_below", "d1", "d2", "created_d1",
                    "created_d2", "created_on", "observed_on", "unobserved_by_user_id", "apply_project_rules_for",
                    "cs", "csa", "csi", "geoprivacy", "taxon_geoprivacy", "hrank", "lrank", "id_above", "id_below", "identifications",
                    "lat", "lng", "radius", "nelat", "nelng", "swlat", "swlng", "list_id", "not_in_project", "not_matching_project_rules_for",
                    "q", "search_on", "quality_grade", "updated_since", "viewer_id", "reviewed", "locale", "preferred_place_id", "ttl")



  res <- httr::GET(api, query=c(fetch,list(page=1)))
  resDF <- fromJSON(httr::content(res, as = "text"),flatten=TRUE)

  if (resDF$total_results < 10001 & resDF$total_results > 200) {

    for (i in 2:(ceiling(resDF$total_results/resDF$per_page))) {
      res.t <- httr::GET(api, query=c(fetch,list(page=i)))
      resDF.t <- jsonlite::fromJSON(httr::content(res.t, as = "text"),flatten=TRUE)
      resDF$results <- dplyr::bind_rows(resDF$results,resDF.t$results)
      Sys.sleep(1)
    }
    iNatDF <- resDF$results
  }

  iNatDF <- resDF$results

  if (resDF$total_results > 10000) {
    stop("This query exceeds 10,000 records. Please narrow down your search terms or consider
breaking your search into multiple queries")
  }

  if (is.data.frame(iNatDF)=="TRUE") {
    cat(nrow(iNatDF), "records fetched")
  }

  if (resDF[1]=="Error") {
    cat("Error code",resDF$status, "- no results generated from query")
  }

  return(iNatDF)

}
