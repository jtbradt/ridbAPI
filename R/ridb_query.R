#' RIDB query function
#'
#' This function constructs an RIDB API request:
#' @param api is one of RIDB APIs
#' @param arg is a query argument
#' @keywords ridb.query
#' @export

ridb.query <- function(api, arg) {

  # Create path:
  path <- paste("api", pkg.env$api.version, api, paste(arg, collapse = "/"), sep = "/")

  # Query FSF API:
  url <- httr::modify_url("https://ridb.recreation.gov/", path = path)
  resp <- httr::GET(url, query = list(apikey = pkg.env$api.key))

  # Stop if no API key provided:
  if (resp$status_code == "401") {
    stop("No API key provided. Please use the set.api.key() function to set your API key.")
  } else if (resp$status_code == "200") {
    # Parse response:
    parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

    # If no data returned, stop query:
    if (length(parsed) == 0) {
      stop(paste0("Query argument '", arg, "' returned no values."))
    }

    # Extract relevant metadata, if returned:
    if (length(parsed) == 2) {
      # Extract total number of records
      total <- parsed$METADATA$RESULTS$TOTAL_COUNT

      # If total records exceed the 50 record limit,submit additional queries:
      if (total > 50) {
        # Set starting values for remaining records:
        startvals <- seq(50, total, 50)

        # Loop through remaining queries to obtain all records:
        parsed.remaining <- lapply(startvals, function(s) {
          # Create path:
          temp.path <- paste("api", pkg.env$api.version, api, paste(arg, collapse = "/"), sep = "/")

          # Query FSF API:
          temp.url <- httr::modify_url("https://ridb.recreation.gov/", path = temp.path)
          temp.resp <- httr::GET(temp.url, query = list(apikey = pkg.env$api.key, offset = s))

          # Parse response:
          temp.parsed <- jsonlite::fromJSON(httr::content(temp.resp, as = "text", encoding = "UTF-8"))

          # Extract data and process geometry data (remove metadata):
          temp.return <- process.geometry(temp.parsed[[1]])

          return(temp.return)
        })

        # Combine parsed returns:
        parsed.remaining[[length(parsed.remaining) + 1]] <- process.geometry(parsed[[1]])
        return.full <- dplyr::bind_rows(parsed.remaining)
      } else {
        return.full <- process.geometry(parsed[[1]])
      }
    } else {
      # If list is returned, reformat:
      if (class(parsed) == "list") {
        return.full <- data.frame(process.geometry(parsed))
      } else {
        return.full <- process.geometry(parsed)
      }
    }

    return(return.full)
  } else {
    stop(paste0("Your query returned the following error code: ", resp$status_code))
  }
}
