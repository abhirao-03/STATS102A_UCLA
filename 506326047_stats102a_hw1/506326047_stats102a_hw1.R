gcd <- function(x, y) {
  # Outputs the GCD of two integers.
  # Args:
  # x: integer
  # y: integer
  # Return:
  # GCD: integer
  
  nums <- c(abs(x), abs(y)) # stores x, y in case of later transformation.
  a <- max(nums) # makes a the higher value and b the lower value
  b <- min(nums)
  
  if (sum(nums %% 1) != 0){
    stop('arguments a and b must be integers!')
  }
  
  if (a %% b == 0) { # if a mod b = 0 then the min of {a,b} is the GCD.
    return(b)
  } else { # follow the algorithm described in HW1.pdf
    mod_xy <- a %% b
    while (mod_xy != 0) {
      mod_xy <- a %% b
      a <- b
      b <- mod_xy
    }
    return(a)
  }
}

lcm <- function(x) {
  # Outputs the LCM of the integers.
  # Args:
  # x: vector    n x 1 dimensional vector where 1 < n < 101
  # Return:
  # LCM: integer  1-dimensional vector

  # We can use the relationship that LCM(a, b) = (a * b)/GCD(a, b)
  # run the code block below till we have a vector with 1 integer value.
  if (sum(x %% 1) != 0){
    stop('x must be a vector of integers!')
  }
  if (length(x) < 2){
    stop('x contains only one element, minimum size for x is 2')
  }
  
  while (length(x) != 1) {
    a <- abs(x[1])
    b <- abs(x[2])
    x <- x[2:length(x)]
    x[1] <- a * b / gcd(a, b)
  }
  x
}


is_prime <- function(x) {
  # The helper function to return a list of bools for primes.
  # Args:
  # x: vector    n x 1 dimensional vector where 1 <= n
  # Return:
  # logical_list: list of bools for each i in x.
  if (sum(x %% 1) != 0){
    stop('x must be a vector of integers!')
  }
  if (sum(x < 0) != 0){
    stop('All elements of x must be positive!')
  }
  
  logical_list <- as.logical(1:length(x))
  for (i in 1:length(x)) {
    if (x[i] == 1 | x[i] == 2) { # handle 1 and 2 differently than other primes.
      logical_list[i] <- TRUE
    } else { # see if any value between 1 and x divides x: if so then not prime.
      for (val in (x[i] %% (2:(x[i] - 1)))) {
        if (val == 0) {
          logical_list[i] <- FALSE
        }
      }
    }
  }
  logical_list
}


exp_count <- function(x, prime) {
  # Returns the exponent required for a given prime number to divide some x.
  # Args:
  # x: integer      the number to which the prime will be raised.
  # prime: integer  the prime number to raise to x.
  # Return:
  # logical_list: list of bools for each i in x.
  if (x %% 1 != 0){
    stop('x must be an integer!')
  }
  
  exp_count <- 0
  while (x %% prime == 0){
    exp_count <- exp_count + 1
    x <- x / prime
  }
  exp_count
}

prime_divisors <- function(x) {
  # Finds the prime divisors of a given integer.
  # Args:
  # x: integer
  # Return
  # prime_list: list   the primes that divide x.
  if (x %% 1 != 0){
    stop('x must be an integer!')
  }
  
  prime_list <- c()
  for (i in 2:x) {
    if (is_prime(i)) {
      if (x %% i == 0) {
        prime_list <- append(prime_list, i)
      }
    }
  }
  prime_list
}


get_factors <- function(x) {
  # Combines the previous two functions to get the prime factors and powers.
  # Args:
  # x:integer
  # Return:
  # factors:list  consisting of 2 nested lists called "primes" and "exponents".
  primes <- prime_divisors(x)
  exponents <- 1:length(primes)
  for (i in 1:length(primes)) {
    p <- primes[i]
    exponents[i] <- exp_count(x, p)
  }
  factors <- list("primes" = primes, "exponents" = exponents)
  factors
}