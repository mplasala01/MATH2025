#' myf
#'
#' @param y Total number of trials (integer)
#' @param r Number of successes (integer)
#' @param p Probability of success in each trial (0 < p < 1)
#' @returns
#' @export
#'
#' @examples
myf=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
