#' @title Creates a coropleth map in leaflet.
#'
#' @description This function allows the creation of a coropleth map with leaflet and sf in a easier way.
#' @param shapefile
#' @param var
#' @param log.scale
#' @param palette
#' @param alpha
#' @param guidetitle
#' @keywords areal data
#' @export

lf_area = function(shapefile, var, log.scale = FALSE, palette = 'YlOrRd', alpha = 0.8,
                   guidetitle = "Guide"){

  if(log.scale){var = log(var + 1)}

  shapefile$var = var

  pal <- leaflet::colorBin(palette, domain = var)

  leaflet::leaflet(shapefile) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(fillColor = ~pal(var), weight = 1, fillOpacity = alpha, color = 'black') %>%
    leaflet::addScaleBar(position = 'bottomleft') %>%
    leaflet::addLegend(pal = pal, values = var, position = 'topleft', title = guidetitle)

}
