#' Euclidean Algorithm
#'
#' Euclidean algorithm finds the greatest common divisor between scalars \code{a} and \code{b}.
#' The algorithm works by... #TODO# 
#'
#' @param a scalar
#' @param b scalar
#'
#' @return Scalar value of the greatest common divisor.
#'
#' @examples
#' euclidean(a, b)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export

euclidean <- function(a,b){
  stopifnot(is.numeric(a), is.numeric(b))
  k = 2
  q = vector()
  r = vector()
  q[1] = a %/% b
  r[1] = a %% b
  
  while(tail(r, 1) != 0){
    a = b
    b = r[k-1]
    q[k] = a %/% b
    r[k] = a %% b
    k = k + 1
  }
  return(b)
}