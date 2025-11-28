#' myncurve
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a stopping point
#'
#' @returns
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  xcurve <- seq(mu-3 * sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(xcurve, a), c(ycurve, 0), col = 'red')

  prob <- pnorm(a, mean = mu, sd = sigma)

  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
