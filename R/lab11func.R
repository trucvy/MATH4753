#' Title
#'
#' @param x data set
#'
#' @return conf interval 95%
#' @export
#'
#' @examples
#' \dontrun{myci(x)}
myci <- function(x){
  n = length(x)
  alpha <- 0.05
  z <- qnorm(1 - alpha/2, 0, 1)
  t <- qt(1-alpha/2, n-1)
  mp <- c(-1,1)
  mean(x) + mp*t*sd(x)/sqrt(n)
}
