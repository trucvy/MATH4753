#' My first function
#'
#' @param x A quantitative vector
#'
#' @return A vector of squared components
#' @export
#'
#' @examples
#' \dontrun{myfun(1:10)}
myfun <- function(x = 2){
  x^2
}

#' @title binomial function
#'
#' @param iter number of iterations
#' @param n number of samples
#' @param p probability
#'
#'
#' @return binomial simulation
#' @export
#'
#' @examples
#' \dontrun{mybin(iter=100,n=10, p=0.5)}
mybin=function(iter = 100,n = 10, p = 0.5){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

#' @title hypergeometric simulation
#'
#' @param iter Number of iterations
#' @param N N
#' @param r gamma
#' @param n n
#'
#' @import graphics
#'
#' @return hyper graph
#' @export
#'
#' @examples
#' \dontrun{myhyper(iter = 1000, N = 20, r = 12, n = 5)}
myhyper =function(iter = 1000, N = 20, r = 12 , n = 5){
  sam.mat=matrix(NA, nrow=n , ncol=iter , byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}


#' Title
#'
#' @param mu mean
#' @param sigma sd
#' @param a x = a
#'
#' @return Show curve and area from -inf to x = a
#' @export
#'
#' @examples myncurve(mu = 0, sigma = 1, a = 2)
myncurve = function(mu = 0, sigma = 1, a = 2) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  xcurve = seq(-1000, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)

  polygon(c(a, xcurve), c(0, ycurve), col = "Purple")

  area = pnorm(a, mu, sigma) - pnorm(-Inf, mu, sigma)
  arear = round(area, 4)

  text(x = mu, y = 0.5*dnorm(mu, mu, sigma), paste0("Area = ", arear))
}

