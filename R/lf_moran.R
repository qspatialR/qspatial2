#' @title Spatial dependency map with leaflet.
#'
#' @description
#' @param shapefile
#' @param var
#' @param binary.mat
#' @param alpha
#' @keywords areal data
#' @export


lf_moran = function(shapefile, var, binary.mat = TRUE, alpha = 0.85){

  nb = spdep::poly2nb(shapefile)
  if(binary.mat == TRUE){wmatrix = spdep::nb2listw(nb, style = "B")} else {wmatrix = spdep::nb2listw(nb, style = "W")}

  lmoran = spdep::localmoran(var, wmatrix)
  shapefile$lmoran = lmoran[,1]
  shapefile$pmoran = lmoran[,5]
  sign = 0.05
  shapefile$pmoran.sig = ifelse(shapefile$pmoran <= sign, "Significant", "Not Significant")
  shapefile$scaled = scale(var)
  shapefile$lagged = spdep::lag.listw(wmatrix, shapefile$scaled)

  shapefile$Moran.Cat = factor(
    ifelse(shapefile$scaled > 0 & shapefile$lagged > 0 & shapefile$pmoran <= sign, "High - High",
           ifelse(shapefile$scaled > 0 & shapefile$lagged < 0 & shapefile$pmoran <= sign, "High - Low",
                  ifelse(shapefile$scaled < 0 & shapefile$lagged > 0 & shapefile$pmoran <= sign, "Low - High",
                         ifelse(shapefile$scaled < 0 & shapefile$lagged < 0 & shapefile$pmoran <= sign, "Low - Low",
                                "Not Significant")))), levels = c("High - High", "High - Low", "Low - High", "Low - Low", "Not Significant"))


  colours = leaflet::colorFactor(palette = c("red","pink","light blue","blue","#e5e5e5"), shapefile$Moran.Cat)

  leaflet::leaflet(shapefile, option=leafletOptions(zoomControl = FALSE)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(fillColor = ~colours(Moran.Cat), weight = 1, fillOpacity = alpha, color = 'black') %>%
    leaflet::addScaleBar(position = 'bottomleft') %>%
    leaflet::addLegend(pal = colours, values = shapefile$Moran.Cat, position = 'topleft', title = 'Spatial Dependence')

}
