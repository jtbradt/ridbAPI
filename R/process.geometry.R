#' Process geometry function
#'
#' This function process geometry returns from the FSF APIs:
#' @param data is the relevant data from parsed API response
#' @keywords process.geometry
#' @export

process.geometry <- function(data) {
  # If returned data has geometry, process:
  if ("GEOJSON" %in% names(data)) {

    # Extract geometry data from parsed response:
    geometry <- data$GEOJSON$COORDINATES

    # Extract geometry type from parsed response:
    type <- data$GEOJSON$TYPE

    # If geometry type is point, process:
    if (unique(type)[unique(type)!=""] %in% c("Point", "point", "POINT")) {
      # If geometry is a list:
      if (class(geometry) == "list") {
        # Set null geometries to c(0,0):
        geometry <- lapply(geometry, function(x){
          if(is.null(x)){
            x <- c(0,0)
          } else {
            x <- x
          }
        })

        # Bind long-lat data and format as data.frame:
        geometry <- data.frame(do.call(rbind, geometry))
      } else {
        geometry <- data.frame(t(geometry))
      }

      # Convert to sfc:
      geometry.sf <- sf::st_set_crs(sf::st_as_sf(geometry, coords = c(1,2)), 4326)

    } else {
      stop("GeoJSON data type not point geometries. Cannot currently process non-point geometries!")
    }

    # Bind processed geometry data to non-geom data:
    if (class(data) == "list") {
      return <- append(data[names(data) != "GEOJSON"], geometry.sf)
    } else {
      return <- dplyr::bind_cols(data[,names(data) != "GEOJSON"], geometry.sf)
    }
  } else {
    return <- data
  }

  return(return)
}


