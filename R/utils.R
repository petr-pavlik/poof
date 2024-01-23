#' Set excluding operator
#' 
#' @description Answers the question whether the LHS given set of objects is within the RHS.
#' @example 
#' 1:3 %out% 2:5
#' 
`%out%` <- Negate("%in%")

#' Pipe to data.table alias
#' 
#' @description Answers the question whether the LHS given set of objects is within the RHS.
#' @example 
#' 1:3 %out% 2:5
#' 
".d(" <- `[`

#' colorbar triangular ends for colorbar
#'
#' @param ... 
#' @description ggplot2 compatible triangular ending for colorbar such as is in ProPlot. 
#' @import ggplot2
#' @import grid
#' @import gtable
#' 
#' @return
#' @export
#'
#' @examples
triangle_vertical_end_colourbar <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("triangle_vertical_end_colourbar", class(guide))
  guide
}

guide_gengrob.triangle_vertical_end_colourbar <- function(...) {
  # First draw normal colourbar
  guide <- NextMethod()
  # Extract bar / colours
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] - 1)
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar])
  
  # Draw triangles
  top <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(0, 1, 0), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  bottom <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(1, 0, 1), "npc"),
    gp = gpar(fill = extremes[2], col = NA)
  )
  # Add triangles to guide
  guide <- gtable_add_grob(
    guide, top, 
    t = guide$layout$t[is_bar] - 1,
    l = guide$layout$l[is_bar]
  )
  guide <- gtable_add_grob(
    guide, bottom,
    t = guide$layout$t[is_bar] + 1,
    l = guide$layout$l[is_bar]
  )
  
  return(guide)
}
