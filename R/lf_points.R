#' @title Creates a point pattern map with leaflet.
#'
#' @description
#' @param data
#' @param x
#' @param y
#' @param crs
#' @param pointsize
#' @param pointcolor
#' @param title
#' @keywords point pattern data
#' @export

lf_points = function(data, x, y, crs = 29193, pointsize = 1,
                     pointcolor = "black", title = 'Point Pattern'){

  # Transforming the coordinates into UTM
  data = sf::st_as_sf(data, coords = c(x, y), crs = crs)
  data_utm = sf::st_transform(data, crs = crs)
  coordinates_utm = sf::st_coordinates(data_utm)

  data = data.frame(long = coordinates_utm[,1], lat = coordinates_utm[,2])

  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(fillOpacity = 0.5, stroke = FALSE, color = 'red', radius = 5)

}
