#' birthday function
#'
#' @param k the number of people in the room
#'
#' @returns Probability
#' @export
#'
#' @examples birthday(20)
birthday <- function(k) {
  1 - exp(lchoose(365, k)+ lfactorial(k)-k*log(365))
}

