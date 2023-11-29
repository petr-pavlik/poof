#' Kling-Gupta criteria wrapper
#'
#' @param x Observed series
#' @param y Simulated series
#' @param type Version of computation
#'
#' @return
#' @export
#' 
#' @description The 
#' @export
#'
#' @examples
KGE <- function(x, y, type = "2011") {
  stopifnot({
    type %in% c("2011", "2012")
    length(x) == length(y)
  })
  switch(type,
         "2011" = {
           .KGE2011cpp(x, y)
         },
         "2012" = {
           .KGE2011cpp(x, y)
         })
}

#' Nash-Sutcliffe criteria wrapper
#'
#' @param x Observed series
#' @param y Simulated series
#' @param type Version of computation
#'
#' @return
#' @export
#' 
#' @description The Nash-Sutcliffe criteria. The wrapper takes care of the errors
#' as well as the argument order in the call
#' @export
#'
#' @examples
NSE <- function(x, y) {
  stopifnot({
    length(x) == length(y)
  })
  .NSEcpp(x, y)
}




