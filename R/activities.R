#' Activities function
#'
#' This function queries the Activities API:
#' @param lookup.type is the type of entity to use to query campsites
#' @param lookup.args is a vector of arguments used to query campsites
#' @keywords activities
#' @export

activities <- function(lookup.type, lookup.args) {
  # If lookup.type is not 'activities', execute query:
  if (lookup.type != "activities") {
    # If lookup.args is null, return error:
    if (is.null(lookup.args)) {
      stop(paste0("Cannot query all '", lookup.type, "' activities; please supply a vector of arguments to query."))
    } else {
      # Execute query:
      query <- lapply(lookup.args, function(a){
        ridb.query(lookup.type, c(a, "activities"))
      })

      # Bind return rows:
      full.query <- do.call(rbind, query)
    }
  } else {
    # Execute query:
    if (is.null(lookup.args)) {
      full.query <- ridb.query(lookup.type, NULL)
    } else {
      query <- lapply(lookup.args, function(a){
        ridb.query(lookup.type, a)
      })

      # Bind return rows:
      full.query <- do.call(rbind, query)
    }
  }

  return(full.query)
}

