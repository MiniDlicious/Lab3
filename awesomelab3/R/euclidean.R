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