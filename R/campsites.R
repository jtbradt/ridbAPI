#' Campsites function
#'
#' This function queries the Campsites API:
#' @param lookup.type is the type of entity to use to query campsites
#' @param lookup.args is a vector of arguments used to query campsites
#' @keywords campsites
#' @export

campsites <- function(lookup.type, lookup.args) {
  # If lookup.type is not 'campsites', execute query:
  if (lookup.type != "campsites") {
    # If lookup.args is null, return error:
    if (is.null(lookup.args)) {
      stop(paste0("Cannot query all '", lookup.type, "' campsites; please supply a vector of arguments to query."))
    } else {
      # Execute query:
      query <- lapply(lookup.args, function(a){
        ridb.query(lookup.type, c(a, "campsites"))
      })
    }
  } else {
    # Execute query:
    query <- lapply(lookup.args, function(a){
      ridb.query(lookup.type, a)
    })
  }

  # Bind return rows:
  full.query <- do.call(rbind, query)

  return(full.query)
}
