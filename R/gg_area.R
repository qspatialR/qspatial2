#' @title Creates a coropleth map in ggplot2
#'
#' @description This function allows the creation of a coropleth map with ggplot2 and sf in a easier way.
#' @param shapefile
#' @param var
#' @param log.scale
#' @param maptitle
#' @param guidetitle
#' @keywords areal data
#' @export

gg_area = function(shapefile, var, log.scale = FALSE, guidetitle = 'Guide', maptitle = 'Areal data'){

  if(log.scale){var = log(var + 1)}

  ggplot2::ggplot() +
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


}
