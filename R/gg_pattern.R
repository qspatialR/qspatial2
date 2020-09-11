#' @title Automated analysis for point pattern data with spatstat and ggplot2.
#'
#' @description
#' @param shapefile
#' @param data
#' @param x
#' @param y
#' @param crs
#' @param fun
#' @param nsim
#' @param pointsize
#' @param pointcolor
#' @param palette
#' @param alpha
#' @param as.list
#' @param title
#' @keywords point pattern data
#' @export


gg_pattern = function(shapefile, data, x, y, crs = 29193, fun = c("G", "F"), nsim = 10, pointsize = 1,
         pointcolor = "black", palette = "RdYlGn", alpha = 0.5, as.list = FALSE){

  # Checking the inputs
  if(length(fun) > 2){stop("Only two summary functions are supported each time.")}
  if(fun[1] %in% c("G", "F", "K", "J") == FALSE){stop("The first summary function chosen is invalid.")}
  if(fun[2] %in% c("G", "F", "K", "J") == FALSE){stop("The second summary function chosen is invalid.")}
  if(fun[1] == fun[2]){stop("Both summary functions are the same.")}


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

  # Creating the ppp object
  data_ppp = suppressMessages(spatstat::ppp(x = coordinates_utm[,1], y = coordinates_utm[,2], window = maptools::as.owin(shapefile)))
  data_ppp = spatstat::as.ppp(data_ppp)
  data_ppp = spatstat::unique.ppp(data_ppp)

  # Visualizing the data
  g1 = ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile,  fill = "#e5e5e5", col = "black") +
    ggplot2::geom_point(aes(x = coordinates_utm[,1], y = coordinates_utm[,2]), size = pointsize, color = pointcolor) +
    ggplot2::coord_sf() +
    ggplot2::theme_light() +
    ggplot2::ggtitle('Point Pattern') +
    ggplot2::ylab("Y") +
    ggplot2::xlab("X")

  # Density map
  g2 = ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile,  fill = "#e5e5e5", col = "black") +
    ggplot2::stat_density2d(aes(x = coordinates_utm[,1], y = coordinates_utm[,2], fill = ..level..), geom = "polygon", alpha = alpha) +
    ggplot2::scale_fill_distiller(palette = palette) +
    ggplot2::theme_light() +
    ggplot2::ggtitle('Intensity') +
    ggplot2::ylab("Y") +
    ggplot2::xlab("X")

  # Summary functions

  # First summary function

  if(fun[1] == "G"){ggt1 = "G function"; yl1 = "G(r)"
  env1 = suppressMessages(spatstat::envelope(data_ppp, Gest, nsim = nsim, verbose = FALSE))
  }

  if(fun[1] == "F"){ggt1 = "F function"; yl1 = "F(r)"
  env1 = suppressMessages(spatstat::envelope(data_ppp, Fest, nsim = nsim, verbose = FALSE))
  }

  if(fun[1] == "K"){ggt1 = "K function"; yl1 = "K(r)"
  env1 = suppressMessages(spatstat::envelope(data_ppp, Kest, nsim = nsim, verbose = FALSE))
  }

  if(fun[1] == "J"){ggt1 = "J function"; yl1 = "J(r)"
  env1 = suppressMessages(spatstat::envelope(data_ppp, Jest, nsim = nsim, verbose = FALSE))
  }

  # Second summary function

  if(fun[2] == "G"){ggt2 = "G function"; yl2 = "G(r)"
  env2 = suppressMessages(spatstat::envelope(data_ppp, Gest, nsim = nsim, verbose = FALSE))
  }

  if(fun[2] == "F"){ggt2 = "F function"; yl2 = "F(r)"
  env2 = suppressMessages(spatstat::envelope(data_ppp, Fest, nsim = nsim, verbose = FALSE))
  }

  if(fun[2] == "K"){ggt2 = "K function"; yl2 = "K(r)"
  env2 = suppressMessages(spatstat::envelope(data_ppp, Kest, nsim = nsim, verbose = FALSE))
  }

  if(fun[2] == "J"){ggt2 = "J function"; yl2 = "J(r)"
  env2 = suppressMessages(spatstat::envelope(data_ppp, Jest, nsim = nsim, verbose = FALSE))
  }

  g3 = ggplot2::ggplot(data.frame(env1), aes(x = r))+
    ggplot2::ggtitle(ggt1) +
    ggplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo - 0.01, ymax = hi + 0.01), alpha = 0.75, fill = 'grey') +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2::ylab(yl1) + xlab("r") +
    ggplot2::theme_light()

  g4 = ggplot2::ggplot(data.frame(env2), aes(x = r))+
    ggplot2::ggtitle(ggt2) +
    ggplot2::geom_line(aes(y = lo), col='grey') +
    ggplot2::geom_line(aes(y = hi), col='grey') +
    ggplot2::geom_ribbon(aes(ymin = lo - 0.01, ymax = hi + 0.01), alpha = 0.75, fill = 'grey') +
    ggplot2::geom_line(aes(y = theo), col = 'red') +
    ggplot2::geom_line(aes(y = obs)) +
    ggplot2:: ylab(yl2) + xlab("r") +
    ggplot2::theme_light()

  # Making a list with the individual plots
  if(as.list == TRUE){return(list(g1, g2, g3, g4))}

  # Plotting the grid
  cowplot::plot_grid(g1, g2, g3, g4, align = "v", axis = "tblr", rel_heights = c(2, 2, 0.8,0.8))

}
