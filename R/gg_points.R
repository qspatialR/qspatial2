#' @title Creates a point pattern map with ggplot2.
#'
#' @description
#' @param shapefile
#' @param data
#' @param x
#' @param y
#' @param crs
#' @param pointsize
#' @param pointcolor
#' @param title
#' @param ylab
#' @param xlab
#' @keywords point pattern data
#' @export

gg_points = function(shapefile, data, x, y, crs = 29193, pointsize = 1,
                      pointcolor = "black", title = 'Point Pattern', ylab = 'Y', xlab = 'X'){

  if(class(shapefile)[1] != "sf"){
    cat("Transforming the shapefile into a sf object using Coordinate Reference System = ",crs,". \n", sep = "")
    shapefile = as(shapefile, 'sf')
  }

  shapefile = suppressMessages(sf::st_set_crs(shapefile, crs))
  shapefile = sf::st_transform(shapefile, crs)

  # Transforming the coordinates into UTM
  data = sf::st_as_sf(data, coords = c(x, y), crs = crs)
  data_utm = sf::st_transform(data, crs = crs)
  coordinates_utm = sf::st_coordinates(data_utm)

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile,  fill = "#e5e5e5", col = "black") +
    ggplot2::geom_point(aes(x = coordinates_utm[,1], y = coordinates_utm[,2]), size = pointsize, color = pointcolor) +
    ggplot2::coord_sf()+
    ggplot2::ggtitle(title) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme_light()

}
