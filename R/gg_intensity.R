#' @title Creates a intensity map for point pattern data with ggplot2.
#'
#' @description
#' @param shapefile
#' @param data
#' @param x
#' @param y
#' @param crs
#' @param alpha
#' @param title
#' @param ylab
#' @param xlab
#' @param palette
#' @keywords point pattern data
#' @export

gg_intensity = function(shapefile, data, x, y, crs = 29193, alpha = 0.5, title = 'Intensity', ylab = 'Y', xlab = 'X', palette = "RdYlGn"){

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
    ggplot2::stat_density2d(aes(x = coordinates_utm[,1], y = coordinates_utm[,2], fill = ..level..), geom = "polygon", alpha = alpha) +
    ggplot2::scale_fill_distiller(palette = palette) +
    ggplot2::theme_light() +
    ggtitle(title) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab)

}
