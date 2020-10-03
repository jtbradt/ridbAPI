#' Activities function
#'
#' This function queries the Activities API:
#' @param activities is the set of activities to retrieve
#' @keywords activities
#' @export

activities <- function(activities = NULL) {
  # If no set of activities supplied, query all activities:
  if (is.null(activities)) {
    query <- ridb.query("activities", NULL)
  } else {
    # Loop over set of activities supplied:
    query <- lapply(activities, function(a) {
      temp.query <- ridb.query("activities", a)
    })
    query <- data.table::rbindlist(query)
  }
}

