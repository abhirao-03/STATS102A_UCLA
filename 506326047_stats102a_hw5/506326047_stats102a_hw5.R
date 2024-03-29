get_sqrt <- function(a, tol, iter_max, verbose = FALSE) {
  
  #Uses Newton's method to get the square root of a real number
  #Args:
  # a:          real positive number
  # tol:        tolerance to stop iteration at
  # iter_max:   the maximum iterations allows
  # verbose:    allows for printing intermediate results.
  #Returns:
  # root:       The square root of a

  
  if (a < 0) {
    stop("Square root is not defined for negative numbers.")
  }
  
  x0 <- a / 2

  for (i in 1:iter_max) {
    x_next <- 0.5 * (x0 + a / x0)
    
    if (verbose) {
      cat(x_next, "\n")
    }
    
    if (abs(x_next - x0) < tol) {
      return(x_next)
    }
    
    x0 <- x_next
  }

  if (verbose) {
    cat("Max iterations reached.\n")
  }
  return(x0)
}


get_abroot <- function(a, root, tol, iter_max, verbose = FALSE) {
  #Uses Newton's method to get an arbitrary root of a real number
  #Args:
  # a:          real positive number
  # root:       the nth root.
  # tol:        tolerance to stop iteration at
  # iter_max:   the maximum iterations allows
  # verbose:    allows for printing intermediate results.
  #Returns:
  # root:       The nth root of a
  
  if (a < 0) {
    stop("Root is not defined for negative numbers.")
  }
  
  # Initial guess
  x0 <- a / root
  
  # Iterate until convergence or max iterations reached
  for (i in 1:iter_max) {
    x_next <- ((root - 1) * x0 + a / (x0^(root - 1))) / root
    
    if (verbose) {
      cat(x_next, "\n")
    }
    
    if (abs(x_next - x0) < tol) {
      return(x_next)
    }
    x0 <- x_next
  }
  
  # If max_iterations reached without convergence
  if (verbose) {
    cat("Max iterations reached.\n")
  }
  return(x0)
}


get_min <- function(f, x0, ...){
  #Uses Newton's method to get an arbitrary root of a real number
  #Args:
  # f:          an expression
  # x0:         initial guess.
  #Returns:
  # min:        the minimum of f
  
  single_deriv <- function(f, ...) {
    list2env(list(...), env = current_env())
    Df = D(f, name = 'x')
    eval(Df)
  }
  
  double_deriv <- function(f, ...) {
    list2env(list(...), env = current_env())
    Df = D(f, name = 'x')
    DDf = D(Df, name = 'x')
    eval(DDf)
  }
  
  max_iter <- 1000
  tol <- 1e-8
  
  x <- x0
  
  for (i in 1:max_iter){
    while (abs(single_deriv(f, ...)) > tol){
      f_d <- single_deriv(f, ...)
      f_dd <- double_deriv(f, ...)
      x <- x - (f_d / f_dd)
    }
    return (x)
  }
  warning("method did not converge")
  return (x)
  
}