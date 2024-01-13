GCD <- function(x, y, ...){
  # Outputs the GCD of two integers.
  #Args:
  #x: integer    1-dimensional vector
  #y: integer    1-dimensional vector
  #Return:
  #gcd: integer  1-dimensional vector
  if (x %% y == 0){
    return (min(c(x, y)))
  }
  else{
    mod_xy <- x %% y
    while (mod_xy != 0){
      mod_xy <-  x %% y
      x <- y
      y <- mod_xy
    }
    return (x)
  }
}

LCM <- function(x){
  while (length(x) != 1){
    a <- abs(x[1])
    b <- abs(x[2])
    x <- x[2:length(x)]
    x[1] <- a * b / GCD(a,b)
  }
  return (x)
}