#' birthday function
#'
#' @param k
#'
#' @returns Probability
#' @export
#'
#' @examples
birthday <- function(k) {
  1 - exp(lchoose(365, k)+ lfactorial(k)-k*log(365))
}

