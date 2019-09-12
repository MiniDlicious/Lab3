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
#' euclidean(10, 100)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export

euclidean <- function(a,b){
  stopifnot(is.numeric(a), is.numeric(b)) # Ensure that the arguments are numeric scalars
  q = vector()    # quotients vector
  r = vector()    # reminders vector
  q[1] = a %/% b  # first quotient value
  r[1] = a %% b   # first reminder value
  k = 2           # index
  
  # while loop
  while(tail(r, 1) != 0){ # while the last reminder isn't equal to 0
    a = b
    b = r[k-1]
    q[k] = a %/% b
    r[k] = a %% b
    k = k + 1
  }
  return(b)  # return last divisor 
}