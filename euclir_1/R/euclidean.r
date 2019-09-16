#' calculate the greatest common divisor of two numbers
#' @param a a number
#' @param b a number
#' @return the greatest common divisor of two numbers
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algrithm}


euclidean <- function(a,b){
  if(!(is.numeric(a) & is.numeric(b))) stop("must input numbers!")
  while (b!=0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
