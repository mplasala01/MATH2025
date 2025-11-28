#' myclt
#'
#' @param n size wanted
#' @param iter number of iterations to run
#'
#' @returns a histogram of w
#' @export
#'
#' @examples
#' \dontrun{w=myclt(n=10,iter=10000)}
myclt=function(n=10,iter=10000){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  invisible(sm)
}
