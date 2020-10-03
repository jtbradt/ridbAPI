#' Facilities function
#'
#' This function queries the Facilities API:
#' @param facilities is a set of facility IDs to query; if null, retrieve all facility IDs
#' @param org is a set of organization IDs for which to retrieve all facilities
#' @param rec.area is a set of RecArea IDs for which to retrieve all facilities
#' @param data is a vector of facility-level data to obtain (e.g., campsites, events, addresses, etc.)
#' @keywords facilities
#' @export

facilities <- function(query.entity, query.id, data) {
  # If query.entity 'facilities', query facilities
  if (query.entity == "facilities") {
    query <- ridb.query("facilities", NULL)
  }
}
