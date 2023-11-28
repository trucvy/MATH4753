#' Title Optimize Ticket Selling
#'
#' @param N capacity of plane
#' @param gamma probability plane will be overbooked
#' @param p probability plane
#'
#' @return a list of recommended tickets to sell using discrete and continuous functions
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=200,gamma = 0.02, p = 0.95)}
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {

  n <- seq(N, N * 1.1)

  # Discrete distribution
  nd <- 1 - gamma - pbinom(q = N, size = n, prob = p)
  ind <- which.min(abs(nd))

  # Continuous distribution
  nc <- 1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  f <- function(n) {abs(1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))))}
  ind2 <- optimize(f, interval = c(N, floor(N + N/10)))$minimum

  # Plotting results for the discrete distribution
  plot(n, nd, type = 'b', main=paste("Objective Vs n to find optimal tickets sold\n", "(", n[ind], ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective", pch = 16, col = "blue")
  abline(h = 0, v = n[ind], lwd = 2, col = "red")

  # Plotting results for the continuous distribution
  plot(n, nc, type = 'l', main=paste("Objective Vs n to find optimal tickets sold\n", "(", ind2, ")", "gamma = ", gamma, "N = ", N, "continuous"), ylab = "Objective", col = "red")
  abline(h = 0, v = ind2, col = "blue", lwd = 2)

  # Printing results
  x <- list(nd = n[ind], nc = ind2, N = N, p = p, gamma = gamma)
  print(x)
}
