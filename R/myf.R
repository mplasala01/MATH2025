#' myf
#'
#' @param y Total number of trials (integer)
#' @param r Number of successes (integer)
#' @param p Probability of success in each trial (0 < p < 1)
#' @returns Percentage event could happen
#' @export
#'
#' @examples myf(20, 3, 0.4)
myf=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
