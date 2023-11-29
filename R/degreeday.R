
#' The degree-day model
#'
#' @description
#' A simplem degree day model. Whenever the observed air temperatture (T) celsius is 
#' lower than a threshold temperature Tt, precipitation is added to the snow storage
#' (accumulation A [mm]). In addition to the accumulation, the liquid water sontent 
#' S_lq (mm) in the snowpack is also calculated. S_lq is calculated accounting
#' for precipitation P (mmm), melt M (mm) and refreezing (R) (mm) and has an upper
#' bound constrained by the water holding capacity `C_WH` (-). Refreezing is detemined
#' by S_lq of the day before, a degree-day factor C_M (mm/day C) and a refreezing
#' factor C_FR (-). Melt is constrined by the preceding accumulation and calculated 
#' using C_M, Tt and T. The contribution to surface runoff Q (mm) is all water that
#' exceeds C_WH of the snow pack. For this study, C_WH was kept constant at a value
#' 0.1.
#' 
#'
#' @param Ta 
#' @param Tt 
#' @param P 
#' @param C_WH 
#'
#' @return
#' @export
#'
#' @examples
degree_day <- function(Ta, Tt, P, C_WH, init_ts = 0) {
  
  M <- C_FR <- C_M <- S_lq <- A <- R <- vector(mode = "numeric", length(Ta))
  
  for (i in 2:length(Ta)) {
    
    if(Ta[i] < Tt) {
      
      R[i] = min(S_lq[i - 1], C_FR * C_M * (Tt - Ta[i]))
      A[i] = A[i - 1] + P[i] + R[i]
      S_lq[i] <- S_lq[i - 1] - R[i]
    
    } else {
      
      M[i] <- min(A[i - 1], C_M * T[i] - Tt)
      A[i] <- A[i - 1] - M[i]
      S_lq[i] <- S_lq[i - 1] + P[i] + M[i]
      
      if (S_lq[i] > C_WH * A[i]) {
        
        Q[i] <- S_lq[i] - C_WH * A[i]
        S_lq[i] <- C_WH * A[i]
      
      }
    }
  }
  data.frame(R)
}



# degree_day(Ta = 1:100, Tt = 20, P = 1:100, C_WH = 2)
