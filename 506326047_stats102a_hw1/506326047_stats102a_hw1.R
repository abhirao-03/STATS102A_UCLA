gcd <- function(x, y, ...){
  # Outputs the GCD of two integers.
  #Args:
  #x: integer   1-dimensional vector
  #y: integer   1-dimensional vector
  #Return:
  #gcd: integer 1-dimensional vector
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