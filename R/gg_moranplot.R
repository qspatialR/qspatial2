#' @title Creates a moran plot using ggplot2.
#'
#' @description desc
#' @param shapefile A sf object to be used on the creation of the spatial structure.
#' @param var A variable containing the spatial data.
#' @param binary.mat Logical. If TRUE then the weights natrix will be binary, if FALSE it will be weighted.
#' @keywords areal data, moran plot
#' @import spdep
#' @import ggplot2
#' @export


gg_moranplot = function(shapefile, var, binary.mat = TRUE){

  if(nrow(shapefile) != length(var)){stop("The length of the data vector must be the same as the number of regions in the shapefile.")}

  nb = poly2nb(shapefile)
  if(binary.mat == TRUE){wmatrix = spdep::nb2listw(nb, style = "B")} else {wmatrix = spdep::nb2listw(nb, style = "W")}

  shapefile$scaled = scale(var)
  shapefile$lagged = spdep::lag.listw(wmatrix, shapefile$scaled)

  mmodel = lm(shapefile$lagged ~ shapefile$scaled)
  gmoran = spdep::moran.test(var, wmatrix)
  lmoran = spdep::localmoran(var, wmatrix)
  shapefile$pmoran = factor(ifelse(lmoran[,5] <= 0.05, 1, 0))

  ggplot2::ggplot(shapefile, aes(x = scaled, y = lagged)) +
    ggplot2::geom_hline(yintercept = 0, color = "#dbdbdb") +
    ggplot2::geom_vline(xintercept = 0, color = "#dbdbdb") +
    ggplot2::geom_point(aes(color = pmoran)) +
    ggplot2::scale_color_manual(values = c('black', 'red'), guide = FALSE) +
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

}

