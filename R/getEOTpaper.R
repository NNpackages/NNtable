#' Get EOT paper size
#'
#' @param orientation The orientation of the page
#' @param font_size the font size. Only applicable for figure
#' @param type Should the output be character widths or figure width in cm.
#' @param figure_size Should the size be defeault or full
#'
#' @return list with paper size
#' @export
#'
#' @examples
#' getEOTpaper("Port", 8, type = "Table")
getEOTpaper <- function(orientation = c("port", "land"), font_size = 8,
                        figure_size = c("defeault", "full"),
                        type = c("table", "listing", "figure")) {

  type <- tolower(type)
  type <- match.arg(type)

  if (type %in% c("table", "listing"))
    return(getEOTpaperString(orientation = orientation, font_size = font_size))

  if (type == "figure")
    return(getEOTpaperFigure(orientation = orientation,  figure_size = figure_size))
}



getEOTpaperString <- function(orientation = c("port", "land"), font_size = 8, current_margins = TRUE) {


  orientation <- tolower(orientation)
  orientation <- match.arg(orientation)


  if (!font_size %in% 8:10) stop("font_size needs to be 8, 9, or 10")

  if (!current_margins) # The old margins are kept 
    EOT.paper.size <-
      data.frame(orientation = rep(c("land", "port"), each = 3),
                 font_size   = rep(c(10, 9, 8), 2),
                 page.length = c(41, 43, 49, 61, 66, 74),
                 page.width  = c(113, 125, 141, 80, 89, 100))

  # if (current_margins)
  #   EOT.paper.size <-
  #     data.frame(orientation = rep(c("land", "port"), each = 3),
  #                font_size   = rep(c(10, 9, 8), 2),
  #                page.length = c(41, 43, 52, 62, 70, 79),
  #                page.width  = c(126, 140, 157, 80, 89, 100))
  
  if (current_margins)
    EOT.paper.size <-
    data.frame(orientation = rep(c("land", "port"), each = 3),
               font_size   = rep(c(10, 9, 8), 2),
               page.length = c(44, 49, 53, 67, 74, 81),
               page.width  = c(125, 140, 157, 80, 89, 100))
  
  as.list(EOT.paper.size[EOT.paper.size$orientation == orientation &
                           EOT.paper.size$font_size == font_size, c("page.length", "page.width")])
}


getEOTpaperFigure <- function(orientation = c("port", "land"), figure_size = c("defeault", "full")) {

  orientation <- tolower(orientation)
  orientation <- match.arg(orientation)

  figure_size <- tolower(figure_size)
  figure_size <- match.arg(figure_size)

  EOT.paper.size <-
    data.frame(orientation = rep(c("land", "port"), each = 2),
               figure_size        = rep( c("defeault", "full"), 2),
               width  = c(20,   20,  17, 17),
               height = c(14.4, 15.2, 12, 21.2))


  as.list(EOT.paper.size[EOT.paper.size$orientation == orientation &
                           EOT.paper.size$figure_size == figure_size, c("width", "height")])
}
