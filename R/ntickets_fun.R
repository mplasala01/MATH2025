#' ntickets_fun
#'
#' @param N Number of Seats
#' @param g gamma
#' @param p percent to show
#'
#' @returns two graphs, discrete and continuous of n
#' @export
#'
#' @examples ntickets()
ntickets <- function(N = 400, g = 0.02, p = 0.95){
  par(mfrow = c(1, 2))
  # Find discrete value
  nd = N
  while(pbinom(q = N, size = nd, prob = p) >= 1 - g){
    nd = nd + 1
  }
  nd = nd - 1  # last valid value

  # Continuous solution
  nc <- uniroot(
    function(x) pnorm((N + 0.5 - x * p) / sqrt(x * p * (1 - p))) - (1 - g),
    lower = N, upper = N * 2
  )$root



  # Discrete plot
  f <- function(x) pbinom(q = N, size = x, prob = p) - (1 - g)
  xx <- seq(nd - 10, nd + 15, by = 1)
  yy <- sapply(xx, function(x) f(round(x)))


  plot(xx, yy,
       pch = 21,
       bg = ifelse(xx == nd, "red", "blue"),
       xlab = "Number of Tickets sold (n)",
       ylab = "Objective",
       main = paste0("Discrete Solution (N=", N, ", g=", g, ", p=", p, ")"))
  abline(h = 0, col = "red", lty = 2)
  arrows(nd, min(yy), nd, 0, lwd = 2, col = "purple", length = 0.1)
  text(nd + 1, min(yy), labels = paste0("nd = ", nd))

  # Continuous Plot


  f_cont <- function(x) pnorm((N + 0.5 - x * p) / sqrt(x * p * (1 - p))) - (1 - g)
  xx <- seq(nc - 5, nc + 30, by = 0.1)
  yy <- f_cont(xx)

  plot(xx, yy, type = "l", lwd = 2, col = "blue",
       xlab = "Number of Tickets sold (n)",
       ylab = "Objective",
       main = paste0("Normal Approximation (N=", N, ", g=", g, ", p=", p, ")"))
  abline(h = 0, col = "red", lty = 2)

  arrows(nd, min(yy), nc, 0, lwd = 2, col = "purple", length = 0.1)
  text(nd + 3, min(yy), labels = paste0("nc = ", nc))

  cat("Discrete solution (nd):", nd, "\n")
  cat("Continuous solution (nc):", nc, "\n")
  cat("Number of seats (N):", N, "\n")
  cat("Probability to show (p):", p, "\n")
  cat("Gamma (g):", g, "\n")

  list(
    N = N,
    p = p,
    g = g,
    nd = nd,
    nc = nc)
}

ntickets(400, 0.02, 0.95)
