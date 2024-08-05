#' Factor to Segments
#'
#' @param fac A factor where each level represents a segment
#'
#' @return A list of vectors, each vector contains the indices of the elements
#' of the corresponding segment
#' @export
#'
#' @examples
#' fac <- factor(c("a", "b", "a", "b", "c", "c"))
#' fac2seg(fac)
fac2seg <- function(fac){
  if(!is.factor(fac))
    stop("Input must be a factor")
  lapply(1:length(levels(fac)),
         function(i) which(fac==levels(fac)[i]))
}
