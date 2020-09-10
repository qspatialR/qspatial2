#' @title Complete spatial analysis for areal data
#'
#' @description
#' @param shapefile
#' @param var
#' @param binary.mat
#' @param maptitle
#' @param guidetitle
#' @param sign
#' @keywords areal data
#' @export

gg_moran = function(shapefile, var, binary.mat = TRUE, maptitle = "Areal Data", guidetitle = "Guide", sign = 0.05){

  if(nrow(shapefile) != length(var)){stop("The length of the data vector must be the same as the number of regions in the shapefile.")}

  nb = poly2nb(shapefile)
  if(binary.mat == TRUE){wmatrix = spdep::nb2listw(nb, style = "B")} else {wmatrix = spdep::nb2listw(nb, style = "W")}


  # Creating the coropleth map

  g1 = ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile, ggplot2::aes(fill = var), col = "black") +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::scale_fill_viridis_c(name = guidetitle)+
    ggplot2::ggtitle(maptitle) +
    theme_light() +
    theme(legend.title = ggplot2::element_text(size = 8, face = "italic"),
          panel.border = ggplot2::element_rect(fill = NA, color = "#dbdbdb", size = ggplot2::rel(1)),
          panel.grid = ggplot2::element_line(colour = "#dbdbdb", linetype = 2),
          panel.background  = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
          axis.text = ggplot2::element_text(size = 8, face = "italic"),
          axis.title = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 8),
          plot.subtitle = ggplot2::element_text(size = 7))

  # Moran's Plot

  shapefile$scaled = scale(var)
  shapefile$lagged = spdep::lag.listw(wmatrix, shapefile$scaled)

  mmodel = lm(shapefile$lagged ~ shapefile$scaled)
  gmoran = spdep::moran.test(var, wmatrix)

  g2 = ggplot2::ggplot(shapefile, aes(x = scaled, y = lagged)) +
    ggplot2::geom_point() +
    ggplot2::theme_light() + ggplot2::xlab("Scaled variable") + ggplot2::ylab("Lagged variable") +
    ggplot2::geom_abline(slope = mmodel$coefficients[2], intercept = mmodel$coefficients[1], linetype = 'dashed', color = "blue") +
    ggplot2::xlim(-max(shapefile$scaled, shapefile$lagged), max(shapefile$scaled, shapefile$lagged))+
    ggplot2::ylim(-max(shapefile$scaled, shapefile$lagged), max(shapefile$scaled, shapefile$lagged)) +
    ggplot2::ggtitle("Moran's Plot") +
    ggplot2::labs(subtitle = paste("Global Moran's I:", round(as.numeric(gmoran$estimate[1]),4)," - p-value:", round(as.numeric(gmoran$p.value), 4), sep="")) +
    theme(legend.title = ggplot2::element_text(size = 8, face = "italic"),
          panel.border = ggplot2::element_rect(fill = NA, color = "#dbdbdb", size = ggplot2::rel(1)),
          panel.grid = ggplot2::element_line(colour = "#dbdbdb", linetype = 2),
          panel.background  = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
          axis.text = ggplot2::element_text(size = 8, face = "italic"),
          axis.title = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 8),
          plot.subtitle = ggplot2::element_text(size = 7))

  # Local Moran

  lmoran = spdep::localmoran(var, wmatrix)
  shapefile$lmoran = lmoran[,1]
  shapefile$pmoran = lmoran[,5]
  shapefile$pmoran.sig = ifelse(shapefile$pmoran <= sign, "Significant", "Not Significant")

  shapefile$Moran.Cat = factor(
    ifelse(shapefile$scaled > 0 & shapefile$lagged > 0 & shapefile$pmoran <= sign, "High - High",
    ifelse(shapefile$scaled > 0 & shapefile$lagged < 0 & shapefile$pmoran <= sign, "High - Low",
    ifelse(shapefile$scaled < 0 & shapefile$lagged > 0 & shapefile$pmoran <= sign, "Low - High",
    ifelse(shapefile$scaled < 0 & shapefile$lagged < 0 & shapefile$pmoran <= sign, "Low - Low", "Not Significant")))),
    levels = c("High - High", "High - Low", "Low - High", "Low - Low", "Not Significant"))

  g3 = ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile, ggplot2::aes(fill = Moran.Cat), col = "black") +
    ggplot2::scale_fill_manual(values = c("red","pink","light blue","blue","#e5e5e5"),
                               drop = F, name = "Moran's Categories")+
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle("Significant areas") +
    theme(legend.title = ggplot2::element_text(size = 8, face = "italic"),
          panel.border = ggplot2::element_rect(fill = NA, color = "#dbdbdb", size = ggplot2::rel(1)),
          panel.grid = ggplot2::element_line(colour = "#dbdbdb", linetype = 2),
          panel.background  = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
          legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
          legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
          axis.text = ggplot2::element_text(size = 8, face = "italic"),
          axis.title = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 8),
          plot.subtitle = ggplot2::element_text(size = 7))

  gridExtra::grid.arrange(g1,g2,g1,g3, ncol = 2)

}
