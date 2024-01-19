gcd <- function(x, y){
  # Outputs the GCD of two integers.
  #Args:
  #x: integer
  #y: integer
  #Return:
  #GCD: integer
  
  nums = c(abs(x), abs(y)) #stores pos x, y in case of later transformation.
  a = max(nums)            #makes a the higher value and b the lower value
  b = min(nums)
  
  if (a %% b == 0){        #if a mod b = 0 then the min of {a,b} is the GCD.
    return (b)
  }
  else{                    #follow the algorithm described in HW1.pdf
    mod_xy <- a %% b
    while (mod_xy != 0){
      mod_xy <-  a %% b
      a <- b
      b <- mod_xy
    }
    return (a)
  }
}

lcm <- function(x){
  #Outputs the LCM of the integers.
  #Args:
  #x: vector    n x 1 dimensional vector where 1 < n < 101
  #Return:
  #LCM: integer  1-dimensional vector

  #We can use the relationship that LCM(a, b) = (a * b)/GCD(a, b)
  #run the code block below till we have a vector with 1 integer value.
  while (length(x) != 1){
    a <- abs(x[1])
    b <- abs(x[2])
    x <- x[2:length(x)]
    x[1] <- a * b / gcd(a,b)
  }
  x
}

is_prime <- function(x){
  logical_list <- as.logical(1:length(x))
  for (i in 1:length(x)){
    if (x[i] == 1 | x[i] == 2){
      logical_list[i] <- TRUE
    } else{
        for (val in (x[i] %% (2:(x[i] - 1)))){
          if (val == 0){
            logical_list[i] <- FALSE
        }
      }
    }
  }
  logical_list
}

## TOY QUESTION 1
## write a function that takes an integer as input >= 2
## and counts how many times 2 divides that integer

exp_count <- function(x, prime){
  exp_count <- 0
  for (i in 1:x){
    if (x %% prime^(i) == 0){
      exp_count <- exp_count + 1
    }
  }
  exp_count
}

## TOY QUESTION 2
## take an integer x as input with x >= 2. return a list of prime numbers that divide x.
prime_divisors <- function(x){
  prime_list <- c()
  for (i in 2:x){
    if (is_prime(i)){
      if (x %% i == 0){
        prime_list <- append(prime_list, i)
      }
    }
  }
  prime_list
}


get_factors <- function(x){
  primes <- prime_divisors(x)
  exponents <- 1:length(primes)
  for (i in 1:length(primes)){
    p = primes[i]
    exponents[i] <- exp_count(x, p)
  }
  factors = list("primes" = primes, "exponents" = exponents)
  factors
}